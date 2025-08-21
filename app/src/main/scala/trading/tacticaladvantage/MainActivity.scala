package trading.tacticaladvantage

import android.content.Intent
import android.content.pm.PackageManager
import android.os.Bundle
import android.view.{View, ViewGroup}
import android.widget._
import androidx.appcompat.app.AlertDialog
import androidx.recyclerview.widget.RecyclerView
import com.sparrowwallet.drongo
import fr.acinq.bitcoin.DeterministicWallet.ExtendedPublicKey
import fr.acinq.bitcoin._
import fr.acinq.eclair._
import fr.acinq.eclair.blockchain.electrum.ElectrumWallet.{GenerateTxResponse, OkOrError, RBFResponse, WalletReady}
import fr.acinq.eclair.blockchain.electrum.{ElectrumWallet, WalletSpec}
import fr.acinq.eclair.blockchain.fee.FeeratePerByte
import immortan._
import immortan.crypto.Tools._
import immortan.sqlite.DbStreams
import immortan.utils.ImplicitJsonFormats._
import immortan.utils._
import org.apmem.tools.layouts.FlowLayout
import rx.lang.scala.Subscription
import scodec.bits.ByteVector
import spray.json._
import trading.tacticaladvantage.BaseActivity.StringOps
import trading.tacticaladvantage.Colors._
import trading.tacticaladvantage.MainActivity._
import trading.tacticaladvantage.R.string._
import trading.tacticaladvantage.utils._

import java.util.TimerTask
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

object MainActivity {
  var filteredBtcAddressInfos: Iterable[BtcAddressInfo] = Nil
  var btcAddressSpec: BtcAddressSpec = BtcAddressSpec(Map.empty, Nil)
  case class BtcAddressSpec(amounts: Map[ExtendedPublicKey, Satoshi], addresses: List[BtcAddressInfo] = Nil) {
    def withBalance: List[BtcAddressInfo] = addresses.filter(address => amounts contains address.pubKey)
  }

  var displayFullIxInfoHistory: Boolean = false
  var alreadySeenBtcTxids: Set[ByteVector32] = Set.empty[ByteVector32]
  case class BtcAccumulator(txids: Set[ByteVector32], txinfos: Vector[BtcInfo] = Vector.empty) {
    def withInfo(info: BtcInfo): BtcAccumulator = BtcAccumulator(txids ++ info.relatedTxids, txinfos :+ info)
  }

  var btcTxInfos: Iterable[BtcInfo] = Nil
  var recentBtcInfos: Iterable[BtcInfo] = Nil
  var allInfos: Seq[ItemDetails] = Nil
}

class MainActivity extends BaseActivity with ExternalDataChecker { me =>
  private[this] lazy val paymentTypeIconIds = List(R.id.btcAddress, R.id.btcIncoming, R.id.btcInBoosted,
    R.id.btcOutBoosted, R.id.btcOutCancelled, R.id.btcOutgoing, R.id.usdtIncoming, R.id.usdtOutgoing)

  private[this] lazy val contentWindow = findViewById(R.id.contentWindow).asInstanceOf[RelativeLayout]
  private[this] lazy val itemsList = findViewById(R.id.itemsList).asInstanceOf[ListView]

  private[this] lazy val expandContainer = getLayoutInflater.inflate(R.layout.frag_expand, null, false)
  private[this] lazy val expand = expandContainer.findViewById(R.id.expand).asInstanceOf[ImageButton]

  lazy val walletCards = new WalletCardsViewHolder
  var openListItems = Set.empty[String]

  // PAYMENT LIST

  def loadRecentInfos: Unit = {
    filteredBtcAddressInfos = Nil
    if (displayFullIxInfoHistory) {
      // Use wishes to see all txs, we still limit by some sensible number to now slow UI down
      recentBtcInfos = WalletApp.btcTxDataBag.listRecentTxs(500).map(WalletApp.btcTxDataBag.toInfo)
      btcTxInfos = recentBtcInfos
    } else {
      val initialAccumulator = BtcAccumulator(alreadySeenBtcTxids)
      recentBtcInfos = WalletApp.btcTxDataBag.listRecentTxs(10).map(WalletApp.btcTxDataBag.toInfo)
      val accumulator2 = recentBtcInfos.take(3).foldLeft(initialAccumulator)(_ withInfo _)

      val accumulator3 = recentBtcInfos.drop(3).foldLeft(accumulator2) {
        case (acc, info) if acc.txids.intersect(info.relatedTxids).nonEmpty => acc.withInfo(info)
        case (acc, info) if !info.isConfirmed && !info.isDoubleSpent => acc.withInfo(info)
        case (acc, info) if alreadySeenBtcTxids.contains(info.txid) => acc.withInfo(info)
        case (acc, _) => acc
      }

      btcTxInfos = accumulator3.txinfos
      alreadySeenBtcTxids ++= accumulator3.txids
    }
  }

  def loadSearchedBtcInfos(query: String): Unit = {
    val query1 = query.replaceAll("\\s", "").toLowerCase
    filteredBtcAddressInfos = btcAddressSpec.addresses.filter(_.identity.toLowerCase contains query1)
    btcTxInfos = WalletApp.btcTxDataBag.searchTransactions(query1).map(WalletApp.btcTxDataBag.toInfo)
  }

  def fillAllInfos: Unit = {
    val pending = WalletApp.pendingBtcInfos.values
    val displayed = pending.toList ++ btcTxInfos ++ filteredBtcAddressInfos
    allInfos = SemanticOrder.makeSemanticOrder(displayed)
  }

  def loadRecent: Unit = {
    loadRecentInfos
    fillAllInfos
  }

  def loadSearch(query: String): Unit = {
    loadSearchedBtcInfos(query)
    fillAllInfos
  }

  val searchWorker: ThrottledWork[String, Unit] = new ThrottledWork[String, Unit] {
    override def work(query: String): Unit = if (query.nonEmpty) loadSearch(query) else loadRecent
    override def process(query: String, searchResultEffect: Unit): Unit = paymentAdapterDataChanged.run
  }

  val paymentsAdapter: BaseAdapter = new BaseAdapter {
    override def getItem(pos: Int): ItemDetails = allInfos(pos)
    override def getItemId(position: Int): Long = position
    override def getCount: Int = allInfos.size

    override def getView(pos: Int, savedView: View, parent: ViewGroup): View = getItem(pos) match { case item =>
      val view = if (null == savedView) getLayoutInflater.inflate(R.layout.frag_payment_line, null) else savedView
      val holder = if (null == view.getTag) new PaymentLineViewHolder(view) else view.getTag.asInstanceOf[PaymentLineViewHolder]

      if (openListItems contains item.identity) holder.expand(item) else holder.collapse(item)
      setVisMany(item.isExpandedItem -> holder.spacer, !item.isExpandedItem -> holder.spacer1)
      holder.currentDetails = item
      holder.updateDetails
      view
    }
  }

  class PaymentLineViewHolder(itemView: View) extends RecyclerView.ViewHolder(itemView) { self =>
    val nonLinkContainer: LinearLayout = itemView.findViewById(R.id.nonLinkContainer).asInstanceOf[LinearLayout]
    val amountAndMeta: RelativeLayout = itemView.findViewById(R.id.amountAndMeta).asInstanceOf[RelativeLayout]
    val extraInfo: FlowLayout = itemView.findViewById(R.id.extraInfo).asInstanceOf[FlowLayout]
    val description: TextView = itemView.findViewById(R.id.description).asInstanceOf[TextView]
    val statusIcon: ImageView = itemView.findViewById(R.id.statusIcon).asInstanceOf[ImageView]
    val statusText: TextView = itemView.findViewById(R.id.statusText).asInstanceOf[TextView]
    val labelIcon: ImageView = itemView.findViewById(R.id.labelIcon).asInstanceOf[ImageView]
    val amount: TextView = itemView.findViewById(R.id.amount).asInstanceOf[TextView]
    val meta: TextView = itemView.findViewById(R.id.meta).asInstanceOf[TextView]

    val spacer: View = itemView.findViewById(R.id.spacer)
    val spacer1: View = itemView.findViewById(R.id.spacer1)
    spacer1.setZ(Float.MaxValue)
    itemView.setTag(this)

    val paymentTypeIconViews: List[View] = paymentTypeIconIds.map(itemView.findViewById)
    val iconMap: Map[Int, View] = paymentTypeIconIds.zip(paymentTypeIconViews).toMap
    var currentDetails: ItemDetails = _
    var lastVisibleIconId: Int = -1

    val paymentCardContainer: View = itemView.findViewById(R.id.paymentCardContainer)
    paymentCardContainer setOnClickListener onButtonTap(ractOnTap)

    // MENU BUTTONS

    def setItemLabel: Unit = {
      val (builder, extraInputLayout, extraInput) = singleInputPopupBuilder
      mkCheckForm(alert => runAnd(alert.dismiss)(proceed), none, builder, dialog_ok, dialog_cancel)
      extraInputLayout.setHint(dialog_set_label)
      showKeys(extraInput)

      def proceed: Unit = {
        (Option(extraInput.getText.toString).map(trimmed).filter(_.nonEmpty), currentDetails) match {
          case (labelOpt, info: BtcInfo) => WalletApp.btcTxDataBag.updDescription(info.description.withNewLabel(labelOpt), info.txid)
          case (labelOpt, info: UsdtInfo) => WalletApp.usdtTxDataBag.updDescription(info.description.withNewLabel(labelOpt), info.hashString)
          case _ =>
        }
      }
    }

    def shareItem: Unit = currentDetails match {
      case info: BtcInfo => info.description.addresses.headOption.foreach(share)
      case info: UsdtInfo => share(info.description.toAddrString)
      case info: BtcAddressInfo => share(info.identity)
    }

    def ractOnTap: Unit = {
      val isVisible = extraInfo.getVisibility == View.VISIBLE
      if (isVisible) collapse(currentDetails) else expand(currentDetails)
    }

    // CPFP / RBF

    def boostCPFP(info: BtcInfo): Unit = info.extPubs.flatMap(ElectrumWallet.specs.get) match {
      case Nil => WalletApp.app.quickToast(error_btc_no_wallet)
      case wallets => doBoostCPFP(wallets, info)
    }

    def doBoostCPFP(specs: Seq[WalletSpec], info: BtcInfo): Unit = {
      val changeSpec = ElectrumWallet.orderByImportance(candidates = specs).head
      val address = changeSpec.data.keys.ewt.textAddress(changeSpec.data.changePubKey)
      val ourPubKeyScript = ElectrumWallet.addressToPubKeyScript(address)

      val fromOutPoints = for (idx <- info.tx.txOut.indices) yield OutPoint(info.tx.hash, idx)
      val receivedMsat = info.receivedSat.toMilliSatoshi

      val sendView = new ChainSendView(specs, badge = None, visibilityRes = -1)
      val blockTarget = WalletApp.feeRates.info.onChainFeeConf.feeTargets.fundingBlockTarget
      val target = WalletApp.feeRates.info.onChainFeeConf.feeEstimator.getFeeratePerKw(blockTarget)
      lazy val feeView = new FeeView[GenerateTxResponse](FeeratePerByte(target), sendView.cpfpView.host) {
        rate = target

        worker = new ThrottledWork[String, GenerateTxResponse] {
          override def work(reason: String): GenerateTxResponse = ElectrumWallet.makeCPFP(specs, fromOutPoints.toSet, ourPubKeyScript, rate)
          override def process(reason: String, response: GenerateTxResponse): Unit = update(response.fee.toMilliSatoshi.asSome, showIssue = false)
          override def error(exc: Throwable): Unit = update(feeOpt = None, showIssue = true)
        }

        override def update(feeOpt: Option[MilliSatoshi], showIssue: Boolean): Unit = UITask {
          val currentAmount = BtcDenom.directedWithSign(incoming = receivedMsat, outgoing = 0L.msat, cardOut, cardIn, cardZero, isIncoming = true)
          val afterAmount = BtcDenom.directedWithSign(feeOpt.map(receivedMsat.-).getOrElse(receivedMsat), 0L.msat, cardOut, cardIn, cardZero, isIncoming = true)
          sendView.cpfpView.cpfpCurrent.secondItem.setText(currentAmount.html)
          sendView.cpfpView.cpfpAfter.secondItem.setText(afterAmount.html)
          updatePopupButton(getPositiveButton(alert), feeOpt.isDefined)
          super.update(feeOpt, showIssue)
        }.run
      }

      def attempt(alert: AlertDialog): Unit = {
        // Transaction could have gotten a confirmation while user was filling a form
        val sanityCheck = ElectrumWallet.doubleSpent(specs.head.data.keys.ewt.xPub, info.tx)
        if (sanityCheck.depth > 0 || sanityCheck.isDoubleSpent) return

        val cpfpBumpOrder = SemanticOrder(info.txid.toHex, System.currentTimeMillis)
        // Only update parent semantic order if it does not already have one, record it BEFORE sending CPFP
        val parentDescWithOrder = info.description.withNewOrderCond(cpfpBumpOrder.copy(order = Long.MinValue).asSome)
        WalletApp.btcTxDataBag.updDescription(parentDescWithOrder, info.txid)

        runInFutureProcessOnUI(ElectrumWallet.makeCPFP(specs, fromOutPoints.toSet, ourPubKeyScript, feeView.rate), onFail) { response =>
          // At this point we have received some response, in this case it can not be failure but then we maybe have a hardware wallet

          proceedWithoutConfirm(alert, response) { signedTx =>
            val desc = PlainBtcDescription(address :: Nil, label = None, cpfpBumpOrder.asSome, cpfpBy = None, cpfpOf = info.txid.asSome)
            runFutureProcessOnUI(broadcastTx(desc, signedTx, response.transferred, sent = Satoshi(0L), response.fee, incoming = 1), onFail) {

              case Some(error) =>
                // We revert the whole description back since CPFP has failed
                WalletApp.btcTxDataBag.updDescription(info.description, info.txid)
                cleanFailedBroadcast(signedTx.txid, error.message)

              case None =>
                // Parent semantic order has already been updated, now we also must update CPFP parent info
                WalletApp.btcTxDataBag.updDescription(parentDescWithOrder.withNewCPFPBy(signedTx.txid), info.txid)
            }
          }
        }
      }

      lazy val alert = {
        val builder = titleBodyAsViewBuilder(getString(cpfp_explain).asDefView, sendView.body)
        mkCheckForm(attempt, none, builder, dialog_ok, dialog_cancel)
      }

      feeView.update(feeOpt = None, showIssue = false)
      feeView.customFeerateOption.performClick
      sendView.defaultView = sendView.cpfpView
      sendView.switchToDefault(alert)
    }

    def boostRBF(info: BtcInfo): Unit = info.extPubs.flatMap(ElectrumWallet.specs.get) match {
      case res if res.size < info.extPubs.size => WalletApp.app.quickToast(error_btc_no_wallet)
      case specs => doBoostRBF(specs, info)
    }

    def doBoostRBF(specs: Seq[WalletSpec], info: BtcInfo): Unit = {
      val changeTo = ElectrumWallet.orderByImportance(candidates = specs).head
      val currentFee = BtcDenom.parsedWithSignTT(info.feeSat.toMilliSatoshi, cardOut, cardIn)

      val sendView = new ChainSendView(specs, badge = None, visibilityRes = -1)
      val blockTarget = WalletApp.feeRates.info.onChainFeeConf.feeTargets.fundingBlockTarget
      val target = WalletApp.feeRates.info.onChainFeeConf.feeEstimator.getFeeratePerKw(blockTarget)
      lazy val feeView: FeeView[RBFResponse] = new FeeView[RBFResponse](FeeratePerByte(target), sendView.rbfView.host) {
        rate = target

        worker = new ThrottledWork[String, RBFResponse] {
          override def process(reason: String, response: RBFResponse): Unit = response.result match {
            case Left(ElectrumWallet.PARENTS_MISSING) => showRbfErrorDesc(rbf_err_parents_missing)
            case Left(ElectrumWallet.FOREIGN_INPUTS) => showRbfErrorDesc(rbf_err_foreign_inputs)
            case Right(res) => update(res.fee.toMilliSatoshi.asSome, showIssue = false)
            case _ => error(new RuntimeException)
          }

          override def work(reason: String): RBFResponse = ElectrumWallet.rbfBump(specs, changeTo, info.tx, rate)
          override def error(exc: Throwable): Unit = update(feeOpt = None, showIssue = true)
        }

        private def showRbfErrorDesc(descRes: Int): Unit = UITask {
          updatePopupButton(getPositiveButton(alert), isEnabled = false)
          super.update(feeOpt = Option.empty, showIssue = false)
          setVis(isVisible = true, sendView.rbfView.rbfIssue)
          sendView.rbfView.rbfIssue.setText(descRes)
        }.run

        override def update(feeOpt: Option[MilliSatoshi], showIssue: Boolean): Unit = UITask {
          updatePopupButton(getPositiveButton(alert), isEnabled = feeOpt.isDefined)
          setVis(isVisible = false, sendView.rbfView.rbfIssue)
          super.update(feeOpt, showIssue)
        }.run
      }

      def attempt(alert: AlertDialog): Unit = {
        // Transaction could have gotten a confirmation while user was filling a form
        val sanityCheck = ElectrumWallet.doubleSpent(specs.head.data.keys.ewt.xPub, info.tx)
        if (sanityCheck.depth > 0 || sanityCheck.isDoubleSpent) return

        val rbfParams = RBFParams(info.txid, BtcDescription.RBF_BOOST)
        val ofOriginalTxid = info.description.rbf.map(_.ofTxid).getOrElse(info.txid).toHex
        val rbfBumpOrder = SemanticOrder(ofOriginalTxid, -System.currentTimeMillis)

        runInFutureProcessOnUI(ElectrumWallet.rbfBump(specs, changeTo, info.tx, feeView.rate), onFail) { responseWrap =>
          // At this point we have received some response, in this case it can not be failure but then we maybe have a hardware wallet
          val response = responseWrap.result.right.get

          proceedWithoutConfirm(alert, response) { signedTx =>
            val desc = PlainBtcDescription(Nil, label = None, rbfBumpOrder.asSome, cpfpBy = None, cpfpOf = None, rbfParams.asSome)
            runFutureProcessOnUI(broadcastTx(desc, signedTx, received = Satoshi(0L), info.sentSat, response.fee, incoming = 0), onFail) {

              case Some(error) =>
                // We revert the whole description back since CPFP has failed
                WalletApp.btcTxDataBag.updDescription(info.description, info.txid)
                cleanFailedBroadcast(signedTx.txid, error.message)

              case None =>
                val parentLowestOrder = rbfBumpOrder.copy(order = Long.MaxValue)
                val parentDesc = info.description.withNewOrderCond(parentLowestOrder.asSome)
                WalletApp.btcTxDataBag.updDescription(parentDesc, info.txid)
            }
          }
        }
      }

      lazy val alert = {
        val builder = titleBodyAsViewBuilder(getString(rbf_boost_explain).asDefView, sendView.body)
        mkCheckForm(attempt, none, builder, dialog_ok, dialog_cancel)
      }

      sendView.rbfView.rbfCurrent.secondItem.setText(currentFee.html)
      feeView.update(feeOpt = Option.empty, showIssue = false)
      feeView.customFeerateOption.performClick
      sendView.defaultView = sendView.rbfView
      sendView.switchToDefault(alert)
    }

    def cancelRBF(info: BtcInfo): Unit = info.extPubs.flatMap(ElectrumWallet.specs.get) match {
      case Nil => WalletApp.app.quickToast(error_btc_no_wallet)
      case specs => doCancelRBF(specs, info)
    }

    def doCancelRBF(specs: Seq[WalletSpec], info: BtcInfo): Unit = {
      val changeSpec = ElectrumWallet.orderByImportance(candidates = specs).head
      val address = changeSpec.data.keys.ewt.textAddress(changeSpec.data.changePubKey)
      val ourPubKeyScript = ElectrumWallet.addressToPubKeyScript(address)

      val sendView = new ChainSendView(specs, badge = None, visibilityRes = -1)
      val blockTarget = WalletApp.feeRates.info.onChainFeeConf.feeTargets.fundingBlockTarget
      val currentFee = BtcDenom.parsedWithSignTT(info.feeSat.toMilliSatoshi, cardOut, cardIn)
      val target = WalletApp.feeRates.info.onChainFeeConf.feeEstimator.getFeeratePerKw(blockTarget)
      lazy val feeView: FeeView[RBFResponse] = new FeeView[RBFResponse](FeeratePerByte(target), sendView.rbfView.host) {
        rate = target

        worker = new ThrottledWork[String, RBFResponse] {
          override def process(reason: String, response: RBFResponse): Unit = response.result match {
            case Left(ElectrumWallet.PARENTS_MISSING) => showRbfErrorDesc(rbf_err_parents_missing)
            case Left(ElectrumWallet.FOREIGN_INPUTS) => showRbfErrorDesc(rbf_err_foreign_inputs)
            case Right(res) => update(res.fee.toMilliSatoshi.asSome, showIssue = false)
            case _ => error(new RuntimeException)
          }

          override def work(reason: String): RBFResponse = ElectrumWallet.rbfReroute(specs, info.tx, rate, ourPubKeyScript)
          override def error(exception: Throwable): Unit = update(feeOpt = None, showIssue = true)
        }

        private def showRbfErrorDesc(descRes: Int): Unit = UITask {
          updatePopupButton(getPositiveButton(alert), isEnabled = false)
          super.update(feeOpt = Option.empty, showIssue = false)
          setVis(isVisible = true, sendView.rbfView.rbfIssue)
          sendView.rbfView.rbfIssue.setText(descRes)
        }.run

        override def update(feeOpt: Option[MilliSatoshi], showIssue: Boolean): Unit = UITask {
          updatePopupButton(getPositiveButton(alert), isEnabled = feeOpt.isDefined)
          setVis(isVisible = false, sendView.rbfView.rbfIssue)
          super.update(feeOpt, showIssue)
        }.run
      }

      def attempt(alert: AlertDialog): Unit = {
        // Transaction could have gotten a confirmation while user was filling a form
        val sanityCheck = ElectrumWallet.doubleSpent(specs.head.data.keys.ewt.xPub, info.tx)
        if (sanityCheck.depth > 0 || sanityCheck.isDoubleSpent) return

        val rbfParams = RBFParams(info.txid, BtcDescription.RBF_CANCEL)
        val ofOriginalTxid = info.description.rbf.map(_.ofTxid).getOrElse(info.txid).toHex
        val rbfBumpOrder = SemanticOrder(ofOriginalTxid, -System.currentTimeMillis)

        runInFutureProcessOnUI(ElectrumWallet.rbfReroute(specs, info.tx, feeView.rate, ourPubKeyScript), onFail) { responseWrap =>
          // At this point we have received some response, in this case it can not be failure but then we maybe have a hardware wallet
          val response = responseWrap.result.right.get

          proceedWithoutConfirm(alert, response) { signedTx =>
            val desc = PlainBtcDescription(addresses = Nil, label = None, rbfBumpOrder.asSome, None, None, rbfParams.asSome)
            runFutureProcessOnUI(broadcastTx(desc, signedTx, info.sentSat - response.fee, sent = 0L.sat, response.fee, incoming = 1), onFail) {

              case Some(error) =>
                // We revert the whole description back since CPFP has failed
                WalletApp.btcTxDataBag.updDescription(info.description, info.txid)
                cleanFailedBroadcast(signedTx.txid, error.message)

              case None =>
                val parentLowestOrder = rbfBumpOrder.copy(order = Long.MaxValue)
                val parentDesc = info.description.withNewOrderCond(parentLowestOrder.asSome)
                WalletApp.btcTxDataBag.updDescription(parentDesc, info.txid)
            }
          }
        }
      }

      lazy val alert = {
        val builder = titleBodyAsViewBuilder(getString(rbf_cancel_explain).asDefView, sendView.body)
        mkCheckForm(attempt, none, builder, dialog_ok, dialog_cancel)
      }

      sendView.rbfView.rbfCurrent.secondItem.setText(currentFee.html)
      feeView.update(feeOpt = Option.empty, showIssue = false)
      feeView.customFeerateOption.performClick
      sendView.defaultView = sendView.rbfView
      sendView.switchToDefault(alert)
    }

    // VIEW RELATED

    def collapse[T <: ItemDetails](item: T): Unit = {
      setVis(isVisible = false, extraInfo)
      extraInfo.removeAllViewsInLayout
      openListItems -= item.identity
      description.setMaxLines(1)
    }

    def expand[T <: ItemDetails](item: T): Unit = {
      setVis(isVisible = true, extraInfo)
      extraInfo.removeAllViewsInLayout
      openListItems += item.identity
      description.setMaxLines(3)

      item match {
        case info: UsdtInfo =>
          if (WalletApp.linkUsdt.data.withRealAddress.size > 1)
            for (wallet <- WalletApp.linkUsdt.data.withRealAddress if wallet isRelatedToInfo info)
              addFlowChip(extraInfo, wallet.label, R.drawable.border_gray, None)

          if (info.feeUsdtString > "0") {
            val fee = Denomination.fiatDirectedWithSign("0", info.feeUsdtString, cardOut, cardIn, isIncoming = false)
            addFlowChip(extraInfo, chipText = getString(popup_fee) format fee, R.drawable.border_gray, None)
          }

          addFlowChip(extraInfo, getString(dialog_set_label), R.drawable.border_yellow)(setItemLabel)
          if (info.isIncoming) addFlowChip(extraInfo, getString(dialog_share_address), R.drawable.border_yellow)(shareItem)

        case info: BtcInfo =>
          val canRBF = !info.isIncoming && !info.isDoubleSpent && !info.isConfirmed && info.description.cpfpOf.isEmpty
          val canCPFP = info.isIncoming && !info.isDoubleSpent && !info.isConfirmed && info.description.rbf.isEmpty && info.description.canBeCPFPd

          if (ElectrumWallet.specs.size > 1)
            for (wallet <- info.extPubs flatMap ElectrumWallet.specs.get)
              addFlowChip(extraInfo, wallet.info.label, R.drawable.border_gray, None)

          val txid = getString(popup_btc_txid).format(info.txidString.short)
          addFlowChip(extraInfo, chipText = txid, R.drawable.border_gray, info.txidString.asSome)

          if (info.feeSat > 0L.sat) {
            val fee = BtcDenom.directedWithSign(0L.msat, info.feeSat.toMilliSatoshi, cardOut, cardIn, cardZero, isIncoming = false)
            addFlowChip(extraInfo, chipText = getString(popup_fee) format fee, R.drawable.border_gray, None)
          }

          for (adr <- findBtcInputAddress(info.tx).headOption if adr.ewt.secrets.nonEmpty) {
            addFlowChip(extraInfo, chipText = getString(sign_sign_message), R.drawable.border_yellow) {
              val msg = getString(sign_sign_message_title).format(adr.identity.short, info.txidString.humanFour)
              bringSignDialog(msg.asDefView, adr)
            }
          }

          addFlowChip(extraInfo, getString(dialog_set_label), R.drawable.border_yellow)(setItemLabel)
          if (canRBF) addFlowChip(extraInfo, getString(dialog_boost), R.drawable.border_yellow)(self boostRBF info)
          if (canRBF) addFlowChip(extraInfo, getString(dialog_cancel), R.drawable.border_yellow)(self cancelRBF info)
          if (canCPFP) addFlowChip(extraInfo, getString(dialog_boost), R.drawable.border_yellow)(self boostCPFP info)
          if (info.isIncoming && info.description.addresses.nonEmpty) addFlowChip(extraInfo, getString(dialog_share_address), R.drawable.border_yellow)(shareItem)

        case info: BtcAddressInfo =>
          btcAddressSpec.amounts.get(info.pubKey) foreach { balance =>
            val amount = BtcDenom.parsedWithSign(balance.toMilliSatoshi, cardIn, cardZero)
            addFlowChip(extraInfo, amount, R.drawable.border_gray, None)
          }

          def doSign: Unit = bringSignDialog(getString(sign_sign_message_notx_title).format(info.identity.short).asDefView, info)
          if (info.ewt.secrets.nonEmpty) addFlowChip(extraInfo, getString(sign_sign_message), R.drawable.border_yellow)(doSign)
          addFlowChip(extraInfo, getString(dialog_share_address), R.drawable.border_yellow)(shareItem)
      }
    }

    def updateDetails: Unit = {
      if (currentDetails.isDoubleSpent) meta setText getString(state_double_spent).html
      else meta setText WalletApp.app.when(currentDetails.date, WalletApp.app.dateFormat).html

      currentDetails match {
        case info: UsdtInfo => setVisMany(info.description.label.isDefined -> labelIcon, true -> nonLinkContainer, true -> amountAndMeta, true -> statusIcon, false -> statusText)
        case info: BtcInfo => setVisMany(info.description.label.isDefined -> labelIcon, true -> nonLinkContainer, true -> amountAndMeta, true -> statusIcon, false -> statusText)
        case _: BtcAddressInfo => setVisMany(false -> labelIcon, true -> nonLinkContainer, false -> amountAndMeta, false -> statusIcon, true -> statusText)
      }

      currentDetails match {
        case info: BtcInfo if info.description.cpfpOf.isDefined => description setText description_cpfp
        case info: BtcInfo if info.description.rbf.exists(_.mode == BtcDescription.RBF_BOOST) => description setText description_rbf_boost
        case info: BtcInfo if info.description.rbf.exists(_.mode == BtcDescription.RBF_CANCEL) => description setText description_rbf_cancel
        case info: UsdtInfo => description setText info.description.label.getOrElse(info.description.toAddrString.short0x.html)
        case info: BtcInfo => description setText info.labelOrAddressOpt.getOrElse(me getString tx_btc).html
        case info: BtcAddressInfo => description setText info.identity.short.html
      }

      currentDetails match {
        case info: UsdtInfo if WalletApp.pendingUsdtInfos.contains(info.description.fromAddrString) => itemView.setAlpha(0.6F)
        case info: BtcInfo if WalletApp.pendingBtcInfos.contains(info.txid) => itemView.setAlpha(0.6F)
        case _ => itemView.setAlpha(1F)
      }

      currentDetails match {
        case info: UsdtInfo => statusIcon setImageResource usdtStatusIcon(info)
        case info: BtcInfo => statusIcon setImageResource btcStatusIcon(info)
        case _ =>
      }

      currentDetails match {
        case info: UsdtInfo if info.isIncoming => setVisibleIcon(id = R.id.usdtIncoming)
        case info: BtcInfo if info.description.cpfpOf.isDefined => setVisibleIcon(id = R.id.btcInBoosted)
        case info: BtcInfo if info.description.rbf.exists(_.mode == BtcDescription.RBF_BOOST) => setVisibleIcon(id = R.id.btcOutBoosted)
        case info: BtcInfo if info.description.rbf.exists(_.mode == BtcDescription.RBF_CANCEL) => setVisibleIcon(id = R.id.btcOutCancelled)
        case info: BtcInfo if info.isIncoming => setVisibleIcon(id = R.id.btcIncoming)
        case _: BtcAddressInfo => setVisibleIcon(id = R.id.btcAddress)
        case _: UsdtInfo => setVisibleIcon(id = R.id.usdtOutgoing)
        case _: BtcInfo => setVisibleIcon(id = R.id.btcOutgoing)
      }

      currentDetails match {
        case info: UsdtInfo => amount.setText(Denomination.fiatDirectedWithSign(info.receivedUsdtString, info.sentUsdtString, cardOut, cardIn, info.isIncoming).html)
        case info: BtcInfo => amount.setText(BtcDenom.directedWithSign(info.receivedSat.toMilliSatoshi, info.sentSat.toMilliSatoshi, cardOut, cardIn, cardZero, info.isIncoming).html)
        case info: BtcAddressInfo =>
          statusText.setText(info.description.label getOrElse "?")
          if (btcAddressSpec.amounts contains info.pubKey) expand(info)
      }
    }

    def setVisibleIcon(id: Int): Unit = if (lastVisibleIconId != id) {
      iconMap.get(lastVisibleIconId).foreach(_ setVisibility View.GONE)
      iconMap.get(id).foreach(_ setVisibility View.VISIBLE)
      lastVisibleIconId = id
    }

    def btcStatusIcon(info: BtcInfo): Int = {
      // Ephemeral tx has no connected wallet while it's being broadcasted
      // User may remove a wallet while related transactions are getting confirmed
      val hasNoWallets = info.extPubs.flatMap(ElectrumWallet.specs.get).isEmpty

      if (info.isConfirmed) R.drawable.done_24
      else if (info.isDoubleSpent) R.drawable.block_24
      else if (hasNoWallets) R.drawable.question_24
      else R.drawable.hourglass_empty_24
    }

    def usdtStatusIcon(info: UsdtInfo): Int = {
      val in = WalletApp.linkUsdt.data.okWallets.get(info.description.toAddrString)
      val out = WalletApp.linkUsdt.data.okWallets.get(info.description.fromAddrString)
      val isConfirmed = in.exists(_.chainTip - info.block >= 20) || out.exists(_.chainTip - info.block >= 10)
      val isUnknown = in.isEmpty && out.isEmpty

      if (info.isDoubleSpent) R.drawable.block_24
      else if (isUnknown) R.drawable.question_24
      else if (isConfirmed) R.drawable.done_24
      else R.drawable.hourglass_empty_24
    }
  }

  // LIST CAPTION CLASS

  class WalletCardsViewHolder {
    val view: LinearLayout = getLayoutInflater.inflate(R.layout.frag_wallet_cards, null).asInstanceOf[LinearLayout]
    val fiatUnitPriceAndChange: TextView = view.findViewById(R.id.fiatUnitPriceAndChange).asInstanceOf[TextView]
    val defaultHeader: LinearLayout = view.findViewById(R.id.defaultHeader).asInstanceOf[LinearLayout]
    val holder: LinearLayout = view.findViewById(R.id.chainCardsContainer).asInstanceOf[LinearLayout]
    val searchField: EditText = view.findViewById(R.id.searchField).asInstanceOf[EditText]
    val spinner: ProgressBar = view.findViewById(R.id.spinner).asInstanceOf[ProgressBar]
    val recentActivity: View = view.findViewById(R.id.recentActivity)
    val manager: WalletCardManager = new WalletCardManager(holder)
    // This means search is off at start
    searchField.setTag(false)

    def makeCards: List[WalletCard] = {
      val btcCards = for (xPub <- ElectrumWallet.specs.keys) yield new BtcWalletCard(me, xPub) {
        override def onWalletTap: Unit = goToWithValue(ClassNames.qrBtcActivityClass, xPub)
      }

      val usdtCards = for (info <- WalletApp.linkUsdt.data.wallets) yield new UsdtWalletCard(me, info.xPriv) {
        override def onWalletTap: Unit = println("usdt")
      }

      val taCardOpt = if (WalletApp.showTaCard) new TaWalletCard(me) {
        override def onWalletTap: Unit = println("ta")
      }.asSome else None

      btcCards ++ usdtCards ++ taCardOpt
    }.toList

    def resetCards: Unit = {
      holder.removeAllViewsInLayout
      manager init makeCards
      updateView
    }

    def updateView: Unit = {
      androidx.transition.TransitionManager.beginDelayedTransition(defaultHeader)
      val change = WalletApp.fiatRates.info.pctDifference(code = WalletApp.fiatCode).getOrElse(default = new String)
      val unitRate = WalletApp.msatInFiatHuman(WalletApp.fiatRates.info.rates, WalletApp.fiatCode, 100000000000L.msat, Denomination.formatFiatShort)
      fiatUnitPriceAndChange.setText(s"₿ ≈ $unitRate $change".html)
      manager.cardViews.foreach(_.updateView)
    }
  }

  // LISTENERS

  private var stateSubscription = Option.empty[Subscription]

  private val chainListener = new WalletEventsListener {
    override def onChainSyncing(start: Int, now: Int, maxValue: Int): Unit = UITask {
      setVis(isVisible = maxValue - now > 2016 * 4, walletCards.spinner)
      walletCards.spinner.setMax(maxValue - start)
      walletCards.spinner.setProgress(now - start)
    }.run

    override def onChainSyncEnded(localTip: Int): Unit = UITask {
      setVis(isVisible = false, walletCards.spinner)
    }.run

    override def onWalletReady(event: WalletReady): Unit =
      DbStreams.next(DbStreams.txDbStream)
  }

  private val fiatRatesListener = new FiatRatesListener {
    def onFiatRates(rates: FiatRatesInfo): Unit = UITask {
      walletCards.fiatUnitPriceAndChange.setAlpha(1F)
      walletCards.updateView
    }.run
  }

  // Lifecycle methods

  override def onNewIntent(intent: Intent): Unit = {
    super.onNewIntent(intent)
    setIntent(intent)
  }

  override def onResume: Unit = runAnd(super.onResume) {
    try ElectrumWallet.connectionProvider.notifyAppAvailable catch none
    val dataOpt = Seq(getIntent.getDataString, getIntent getStringExtra Intent.EXTRA_TEXT).find(externalData => null != externalData)
    runInFutureProcessOnUI(dataOpt foreach InputParser.recordValue, none)(_ => try checkExternalData(noneRunnable) catch none)
    setIntent(new Intent)
  }

  override def onDestroy: Unit = {
    try ElectrumWallet.catcher ! WalletEventsCatcher.Remove(chainListener) catch none
    try WalletApp.fiatRates.listeners -= fiatRatesListener catch none
    stateSubscription.foreach(_.unsubscribe)
    super.onDestroy
  }

  type GrantResults = Array[Int]
  override def onRequestPermissionsResult(reqCode: Int, permissions: Array[String], results: GrantResults): Unit = {
    if (reqCode == scannerRequestCode && results.nonEmpty && results.head == PackageManager.PERMISSION_GRANTED) bringScanner(null)
  }

  override def checkExternalData(whenNone: Runnable): Unit = {
    val spendable = ElectrumWallet.specs.values.filter(_.spendable).toList
    val usable = ElectrumWallet.specs.values.filter(_.usable).toList

    def bringSingleAddressSelector(bitcoinUri: BitcoinUri) = new WalletSelector(me titleViewFromUri bitcoinUri) {
      def onOk: Unit = bringSendBitcoinPopup(chosenCards.toList, bitcoinUri)
    }

    def bringMultiAddressSelector(a2a: MultiAddressParser.AddressToAmount) = new WalletSelector(me getString dialog_send_btc_many) {
      def onOk: Unit = bringSendMultiBitcoinPopup(chosenCards.toList, a2a)
    }

    InputParser.checkAndMaybeErase {
      case bitcoinUri: BitcoinUri if Try(ElectrumWallet addressToPubKeyScript bitcoinUri.address).isSuccess =>
        if (spendable.size == 1) bringSendBitcoinPopup(spendable, bitcoinUri)
        else if (usable.size == 1) bringSendBitcoinPopup(usable, bitcoinUri)
        else bringSingleAddressSelector(bitcoinUri)

      case a2a: MultiAddressParser.AddressToAmount =>
        val dustAmount = a2a.values.secondItems.find(amount => ElectrumWallet.params.dustLimit > amount)
        val badAddress = a2a.values.firstItems.find(address => Try(ElectrumWallet addressToPubKeyScript address).isFailure)

        if (badAddress.nonEmpty) onFail(s"Incorrect address=${badAddress.get}")
        else if (dustAmount.nonEmpty) onFail(s"Low amount=${dustAmount.get.toLong}")
        else if (spendable.size == 1) bringSendMultiBitcoinPopup(spendable, a2a)
        else if (usable.size == 1) bringSendMultiBitcoinPopup(usable, a2a)
        else bringMultiAddressSelector(a2a)

      case data: BIP322VerifyData =>
        val address = drongo.address.Address.fromString(drongoNetwork, data.address)
        val verifies = try drongo.crypto.Bip322.verifyHashBip322(address.getScriptType, address, data.messageHash.toArray, data.signature64) catch { case _: Throwable => false }
        val isSignatureLegit = verifies && data.message.map(drongo.crypto.Bip322.getBip322MessageHash).map(ByteVector.view).forall(messageHash => data.messageHash == messageHash)
        val title = if (isSignatureLegit) new TitleView(me getString verify_ok).asColoredView(R.color.buttonGreen) else new TitleView(me getString verify_no).asColoredView(android.R.color.holo_red_light)
        val bld = new AlertDialog.Builder(me).setCustomTitle(title).setMessage(getString(verify_details).format(data.address.humanFour, data.messageHash.toHex.humanFour, data.message getOrElse "?").html)
        mkCheckForm(_.dismiss, share(data.serialize), bld, dialog_ok, dialog_share)

      case data: BIP32SignData =>
        runInFutureProcessOnUI(getBtcAddressSpec.addresses.find(addressInfo => data.address == addressInfo.identity), onFail) {
          case Some(info) => bringSignDialog(getString(sign_sign_message_notx_title).format(info.identity.short).asDefView, info).setText(data.message)
          case None => WalletApp.app.quickToast(sign_address_not_found)
        }

      case _ =>
        whenNone.run
    }
  }

  override def onBackPressed: Unit = {
    if (isSearchOn) rmSearch(view = null)
    else if (displayFullIxInfoHistory) {
      displayFullIxInfoHistory = false
      allInfos = allInfos.take(n = 3)
      paymentAdapterDataChanged.run
    } else super.onBackPressed
  }

  def isSearchOn: Boolean = {
    walletCards.searchField.getTag.asInstanceOf[Boolean]
  }

  override def START(state: Bundle): Unit =
    WalletApp.isAlive match {
      case true if WalletApp.isOperational =>
        setContentView(R.layout.activity_main)

        WalletApp.fiatRates.listeners += fiatRatesListener
        ElectrumWallet.catcher ! chainListener

        itemsList.addFooterView(expandContainer)
        itemsList.addHeaderView(walletCards.view)
        itemsList.setAdapter(paymentsAdapter)
        itemsList.setDividerHeight(0)
        itemsList.setDivider(null)

        expand setOnClickListener onButtonTap {
          androidx.transition.TransitionManager.beginDelayedTransition(contentWindow)
          runAnd(displayFullIxInfoHistory = true)(action = loadRecent)
          paymentAdapterDataChanged.run
        }

        walletCards.resetCards
        walletCards.searchField addTextChangedListener onTextChange(searchWorker.addWork)
        runInFutureProcessOnUI(loadRecent, none) { _ => paymentAdapterDataChanged.run }

        // STREAMS

        val window = 500.millis
        timer.scheduleAtFixedRate(paymentAdapterDataChanged, 30000, 30000)
        stateSubscription = Rx.uniqueFirstAndLastWithinWindow(DbStreams.txDbStream, window).subscribe { _ =>
          // After each delayed update we check if pending txs got confirmed or double-spent
          // do this check specifically after updating txInfos with new items
          if (!isSearchOn) loadRecent

          for {
            txInfo <- recentBtcInfos if !txInfo.isDoubleSpent && !txInfo.isConfirmed
            relatedSpec <- txInfo.extPubs.flatMap(ElectrumWallet.specs.get).headOption
            doubleSpent = ElectrumWallet.doubleSpent(relatedSpec.data.keys.ewt.xPub, txInfo.tx)
            if doubleSpent.depth != txInfo.depth || doubleSpent.isDoubleSpent != txInfo.isDoubleSpent
          } WalletApp.btcTxDataBag.updStatus(txInfo.txid, doubleSpent.depth, doubleSpent.stamp, doubleSpent.isDoubleSpent)

          UITask(walletCards.updateView).run
          paymentAdapterDataChanged.run
        }.asSome

      case true =>
        WalletApp.extDataBag.tryGetSecret match {
          case Failure(_: android.database.CursorIndexOutOfBoundsException) =>
            // Record is not present at all, this is probaby a fresh wallet
            me exitTo classOf[SetupActivity]

          case Failure(reason) =>
            // Notify user about it
            throw reason

          case Success(secret) =>
            WalletApp.makeOperational(secret)
            WalletApp.initWallets
            START(state)
        }

      case false =>
        WalletApp.makeAlive
        START(state)
    }

  // VIEW HANDLERS

  def bringSearch(view: View): Unit = {
    walletCards.searchField.setTag(true)
    androidx.transition.TransitionManager.beginDelayedTransition(contentWindow)
    setVisMany(false -> walletCards.defaultHeader, true -> walletCards.searchField)
    showKeys(walletCards.searchField)

    runInFutureProcessOnUI(getBtcAddressSpec, none) { spec =>
      filteredBtcAddressInfos = spec.withBalance
      btcTxInfos = recentBtcInfos.take(3)
      btcAddressSpec = spec
      fillAllInfos

      // Update view after filling
      paymentAdapterDataChanged.run
    }
  }

  def rmSearch(view: View): Unit = {
    walletCards.searchField.setTag(false)
    walletCards.searchField.setText(new String)
    androidx.transition.TransitionManager.beginDelayedTransition(contentWindow)
    setVisMany(true -> walletCards.defaultHeader, false -> walletCards.searchField)
    WalletApp.app.hideKeys(walletCards.searchField)
  }

  def bringPasteAddressDialog: Unit = {
    def doBringPasteAddressDialog: Unit = {
      val (builder, extraInputLayout, extraInput) = singleInputPopupBuilder
      mkCheckForm(alert => runAnd(alert.dismiss)(proceed), none, builder, dialog_ok, dialog_cancel)
      extraInputLayout.setHint(typing_hints)
      showKeys(extraInput)

      def proceed: Unit = runInFutureProcessOnUI(InputParser recordValue extraInput.getText.toString, onFail) { _ =>
        def attemptProcessInput: Unit = runAnd(doBringPasteAddressDialog)(nothingUsefulTask.run)
        me checkExternalData UITask(attemptProcessInput)
      }
    }

    doBringPasteAddressDialog
  }

  def bringScanner(view: View): Unit = {
    def onCreated(sheet: sheets.OnceBottomSheet) = {
      sheet.altAction setOnClickListener onButtonTap {
        timer.schedule(UITask(bringPasteAddressDialog), 225)
        sheet.dismiss
      }

      setVisMany(true -> sheet.instruction, true -> sheet.altAction)
      sheet.instruction.setText(typing_hints)
      sheet.altAction.setText(dialog_paste)
    }

    val onScan = UITask(me checkExternalData nothingUsefulTask)
    val sheet = new sheets.OnceBottomSheet(me, onCreated, onScan)
    callScanner(sheet)
  }

  def enterSettingsMode(view: View): Unit = {
    println("settings mode on")
  }

  def bringSendBitcoinPopup(specs: Seq[WalletSpec], uri: BitcoinUri): Unit = {
    val sendView = new ChainSendView(specs, getString(dialog_set_label).asSome, dialog_visibility_private)
    val pubKeyScript = ElectrumWallet.addressToPubKeyScript(uri.address)
    val changeTo = ElectrumWallet.orderByImportance(specs).head

    def attempt(alert: AlertDialog): Unit =
      runInFutureProcessOnUI(ElectrumWallet.makeTx(specs, changeTo, pubKeyScript, sendView.manager.resultMsat.truncateToSatoshi, Map.empty, feeView.rate), onFail) { response =>
        // This may be a signing or a hardware wallet, in case if it's a hardware wallet we need additional UI action so we use this proxy method here

        proceedConfirm(sendView, alert, response) { signedTx =>
          val desc = PlainBtcDescription(uri.address :: Nil, sendView.manager.resultExtraInput orElse uri.label orElse uri.message)
          val broadcastFuture = broadcastTx(desc, signedTx, received = Satoshi(0L), sent = response.transferred, response.fee, incoming = 0)
          runFutureProcessOnUI(broadcastFuture, onFail) { case Some(error) => cleanFailedBroadcast(signedTx.txid, error.message) case None => }
          alert.dismiss
        }
      }

    lazy val alert = {
      val title = titleViewFromUri(uri)
      val neutralRes = if (uri.amount.isDefined) -1 else dialog_max
      val builder = titleBodyAsViewBuilder(title.asColoredView(R.color.cardBitcoinSigning), sendView.body)
      def useMax(alert: AlertDialog): Unit = sendView.manager.updateText(specs.map(_.info.lastBalance).sum.toMilliSatoshi)
      mkCheckFormNeutral(attempt, none, useMax, builder, dialog_ok, dialog_cancel, neutralRes)
    }

    lazy val feeView = new FeeView[GenerateTxResponse](FeeratePerByte(1L.sat), sendView.chainEditView.host) {
      rate = WalletApp.feeRates.info.onChainFeeConf.feeEstimator.getFeeratePerKw(WalletApp.feeRates.info.onChainFeeConf.feeTargets.mutualCloseBlockTarget)

      worker = new ThrottledWork[String, GenerateTxResponse] {
        // This is a generic sending facility which may send to non-segwit, so always use a safer high dust threshold
        override def work(reason: String): GenerateTxResponse = ElectrumWallet.makeTx(specs, changeTo, pubKeyScript, sendView.manager.resultMsat.truncateToSatoshi, Map.empty, rate)
        override def error(exception: Throwable): Unit = update(feeOpt = None, showIssue = sendView.manager.resultMsat >= ElectrumWallet.params.dustLimit)
        override def process(reason: String, response: GenerateTxResponse): Unit = update(response.fee.toMilliSatoshi.asSome, showIssue = false)
      }

      override def update(feeOpt: Option[MilliSatoshi], showIssue: Boolean): Unit = UITask {
        updatePopupButton(getPositiveButton(alert), isEnabled = feeOpt.isDefined)
        super.update(feeOpt, showIssue)
      }.run
    }

    // Automatically update a candidate transaction each time user changes amount value
    sendView.manager.inputAmount addTextChangedListener onTextChange(feeView.worker.addWork)
    feeView.update(feeOpt = None, showIssue = false)

    uri.amount foreach { asked =>
      sendView.manager.updateText(value = asked)
      sendView.manager.inputAmount.setEnabled(false)
      sendView.manager.fiatInputAmount.setEnabled(false)
    }
  }

  def bringSendMultiBitcoinPopup(specs: Seq[WalletSpec], addressToAmount: MultiAddressParser.AddressToAmount): Unit = {
    val scriptToAmount = addressToAmount.values.firstItems.map(ElectrumWallet.addressToPubKeyScript).zip(addressToAmount.values.secondItems).toMap
    val sendView = new ChainSendView(specs, badge = None, visibilityRes = -1)
    val changeTo = ElectrumWallet.orderByImportance(specs).head

    def attempt(alert: AlertDialog): Unit =
      runInFutureProcessOnUI(ElectrumWallet.makeBatchTx(specs, changeTo, scriptToAmount, feeView.rate), onFail) { response =>
        // This may be a signing or a hardware wallet, in case if it's a hardware wallet we need additional UI action so we use this method here

        proceedConfirm(sendView, alert, response) { signedTx =>
          val desc = PlainBtcDescription(addressToAmount.values.firstItems.toList)
          val broadcastFuture = broadcastTx(desc, signedTx, received = Satoshi(0L), sent = response.transferred, response.fee, incoming = 0)
          runFutureProcessOnUI(broadcastFuture, onFail) { case Some(error) => cleanFailedBroadcast(signedTx.txid, error.message) case None => }
          alert.dismiss
        }
      }

    lazy val alert = {
      val view = new TitleView(me getString dialog_send_btc_many).asColoredView(R.color.cardBitcoinSigning)
      mkCheckForm(attempt, none, titleBodyAsViewBuilder(view, sendView.body), dialog_ok, dialog_cancel)
    }

    lazy val feeView = new FeeView[GenerateTxResponse](FeeratePerByte(1L.sat), sendView.chainEditView.host) {
      rate = WalletApp.feeRates.info.onChainFeeConf.feeEstimator.getFeeratePerKw(WalletApp.feeRates.info.onChainFeeConf.feeTargets.mutualCloseBlockTarget)

      worker = new ThrottledWork[String, GenerateTxResponse] {
        override def work(reason: String): GenerateTxResponse = ElectrumWallet.makeBatchTx(specs, changeTo, scriptToAmount, rate)
        override def process(reason: String, response: GenerateTxResponse): Unit = update(response.fee.toMilliSatoshi.asSome, showIssue = false)
        override def error(exception: Throwable): Unit = update(feeOpt = None, showIssue = true)
      }

      override def update(feeOpt: Option[MilliSatoshi], showIssue: Boolean): Unit = UITask {
        updatePopupButton(getPositiveButton(alert), isEnabled = feeOpt.isDefined)
        super.update(feeOpt, showIssue)
      }.run
    }

    for (address \ amount <- addressToAmount.values.reverse) {
      val humanAmount = BtcDenom.parsedWithSignTT(amount.toMilliSatoshi, cardIn, cardZero)
      val parent = getLayoutInflater.inflate(R.layout.frag_two_sided_item, null)
      new TwoSidedItem(parent, address.short.html, humanAmount.html)
      sendView.chainEditView.host.addView(parent, 0)
    }

    // Hide address facility, we display a list of addresses instead
    setVis(isVisible = false, sendView.chainEditView.inputChain)
    feeView.update(feeOpt = None, showIssue = false)
    feeView.worker addWork "MULTI-SEND-INIT-CALL"
  }

  def bringSignDialog(title: LinearLayout, info: BtcAddressInfo): EditText = {
    val (container, extraInputLayout, extraInput, extraOption, extraOptionText) = singleInputPopup
    mkCheckForm(alert => runAnd(alert.dismiss)(proceed), none, titleBodyAsViewBuilder(title, container), sign_sign, dialog_cancel)
    extraInputLayout.setHint(sign_message)
    showKeys(extraInput)

    setVisMany(true -> extraOptionText, true -> extraOption)
    extraOptionText.setText(sign_sig_only_info)
    extraOption.setText(sign_sig_only)

    def proceed: Unit = {
      val message = extraInput.getText.toString.trim
      val hash = drongo.crypto.Bip322.getBip322MessageHash(message)
      val messageOpt = if (extraOption.isChecked) None else Some(message)
      val scriptType = drongo.address.Address.fromString(drongoNetwork, info.identity).getScriptType
      val ecKey = drongo.crypto.ECKey.fromPrivate(info.ewt.extPrivKeyFromPub(info.pubKey).privateKey.value.toArray)
      val data = BIP322VerifyData(info.identity, ByteVector.view(hash), drongo.crypto.Bip322.signMessageBip322(scriptType, message, ecKey), messageOpt)
      goToWithValue(ClassNames.qrSigActivityClass, data)
    }

    extraInput
  }

  def paymentAdapterDataChanged: TimerTask = UITask {
    val expandHideCases = displayFullIxInfoHistory || isSearchOn || recentBtcInfos.size <= allInfos.size
    setVisMany(allInfos.nonEmpty -> walletCards.recentActivity, !expandHideCases -> expandContainer)
    paymentsAdapter.notifyDataSetChanged
  }

  def proceedConfirm(sendView: ChainSendView, alert: AlertDialog, response: GenerateTxResponse)(process: Transaction => Unit): Unit = {
    val finalSendButton = sendView.chainConfirmView.chainButtonsView.chainNextButton
    finalSendButton setOnClickListener onButtonTap(process apply response.tx)
    sendView.switchToConfirm(alert, response)
  }

  def proceedWithoutConfirm(alert: AlertDialog, response: GenerateTxResponse)(process: Transaction => Unit): Unit = {
    process(response.tx)
    alert.dismiss
  }

  def broadcastTx(desc: BtcDescription, finalTx: Transaction, received: Satoshi, sent: Satoshi, fee: Satoshi, incoming: Int): Future[OkOrError] = {
    val info = BtcInfo(finalTx.toString, finalTx.txid.toHex, invalidPubKey.toString, depth = 0, received, sent, fee, seenAt = System.currentTimeMillis,
      updatedAt = System.currentTimeMillis, desc, 0L.msat, WalletApp.fiatRates.info.rates.toJson.compactPrint, incoming, doubleSpent = 0)
    WalletApp.pendingBtcInfos(finalTx.txid) = info
    WalletApp.seenBtcInfos(finalTx.txid) = info
    DbStreams.next(DbStreams.txDbStream)
    ElectrumWallet.broadcast(finalTx)
  }

  def cleanFailedBroadcast(failedTxid: ByteVector32, message: String): Unit = {
    WalletApp.pendingBtcInfos.remove(failedTxid)
    WalletApp.seenBtcInfos.remove(failedTxid)
    DbStreams.next(DbStreams.txDbStream)
    onFail(message)
  }

  def getBtcAddressSpec = {
    val amounts = Map.empty[ExtendedPublicKey, Satoshi]
    val amounts1 = ElectrumWallet.specs.values.flatMap(_.data.utxos).foldLeft(amounts) {
      case (acc, utxo) => acc.updated(utxo.key, acc.getOrElse(utxo.key, 0L.sat) + utxo.item.value.sat)
    }

    val addresses = for {
      spec <- ElectrumWallet.specs.values.toList
      description = BtcAddressDescription(spec.info.label.asSome)
      (_, extPubKey) <- spec.data.keys.accountKeyMap ++ spec.data.keys.changeKeyMap
    } yield BtcAddressInfo(spec.data.keys.ewt, spec.info, extPubKey, description)
    BtcAddressSpec(amounts1, addresses)
  }

  def findBtcInputAddress(tx: Transaction): Stream[BtcAddressInfo] = {
    // Using stream to make computation lazy and don't overcompute
    // can be used to take the first found value

    for {
      txIn <- tx.txIn.toStream
      spec <- ElectrumWallet.specs.valuesIterator
      extPubKey <- spec.data.extPubKeyFromInput(txIn)
      description = BtcAddressDescription(spec.info.label.asSome)
    } yield BtcAddressInfo(spec.data.keys.ewt, spec.info, extPubKey, description)
  }

  def drongoNetwork = ElectrumWallet.chainHash match {
    case Block.LivenetGenesisBlock.hash => drongo.Network.MAINNET
    case Block.TestnetGenesisBlock.hash => drongo.Network.TESTNET
    case _ => drongo.Network.REGTEST
  }
}