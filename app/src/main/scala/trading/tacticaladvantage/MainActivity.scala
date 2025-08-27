package trading.tacticaladvantage

import android.content.Intent
import android.content.pm.PackageManager
import android.os.Bundle
import android.view.{View, ViewGroup}
import android.widget._
import androidx.appcompat.app.AlertDialog
import androidx.recyclerview.widget.RecyclerView
import com.sparrowwallet.drongo
import fr.acinq.bitcoin._
import fr.acinq.eclair._
import org.web3j.crypto.Keys.toChecksumAddress
import fr.acinq.eclair.blockchain.electrum.ElectrumWallet.{GenerateTxResponse, OkOrError, RBFResponse, WalletReady}
import fr.acinq.eclair.blockchain.electrum.{ElectrumWallet, WalletSpec}
import fr.acinq.eclair.blockchain.fee.FeeratePerByte
import immortan._
import Tools._
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
  case class BtcAddressAndPrivKey(address: String, privKey: ByteVector32)
  case class Accumulator(identities: Set[String], infos: Set[ItemDetails] = Set.empty) {
    def withBtcInfo(info: BtcInfo): Accumulator = Accumulator(identities ++ info.relatedTxids, infos + info)
    def withUsdtInfo(info: UsdtInfo): Accumulator = Accumulator(identities + info.identity, infos + info)
  }

  val ITEMS = 3
  var displayFullIxInfoHistory: Boolean = false
  var idsToDisplayAnyway: Set[String] = Set.empty

  var usdtInfosToConsider: Iterable[UsdtInfo] = Nil
  var btcInfosToConsider: Iterable[BtcInfo] = Nil

  var infosFromDb: Iterable[ItemDetails] = Nil
  var allInfos: Seq[ItemDetails] = Nil
}

class MainActivity extends BaseActivity with ExternalDataChecker { me =>
  private[this] lazy val contentWindow = findViewById(R.id.contentWindow).asInstanceOf[RelativeLayout]
  private[this] lazy val itemsList = findViewById(R.id.itemsList).asInstanceOf[ListView]

  private[this] lazy val expandContainer = getLayoutInflater.inflate(R.layout.frag_expand, null, false)
  private[this] lazy val expand = expandContainer.findViewById(R.id.expand).asInstanceOf[ImageButton]

  private[this] lazy val paymentTypeIconIds =
    List(R.id.btcAddress, R.id.btcIncoming, R.id.btcInBoosted, R.id.btcOutBoosted,
      R.id.btcOutCancelled, R.id.btcOutgoing, R.id.usdtIncoming, R.id.usdtOutgoing)

  lazy val walletCards = new WalletCardsViewHolder
  var openListItems = Set.empty[String]

  // PAYMENT LIST

  def loadRecentInfos: Unit = {
    usdtInfosToConsider = WalletApp.usdtTxDataBag.listRecentTxs(10).map(WalletApp.usdtTxDataBag.toInfo)
    btcInfosToConsider = WalletApp.btcTxDataBag.listRecentTxs(10).map(WalletApp.btcTxDataBag.toInfo)

    if (displayFullIxInfoHistory) {
      // Simply display everything we obtained from db
      infosFromDb = usdtInfosToConsider ++ btcInfosToConsider
    } else {
      // First, we order all items chronologically to pick 3 recent ones
      val candidates = usdtInfosToConsider.take(ITEMS) ++ btcInfosToConsider.take(ITEMS)
      val sortedCandidates = candidates.toList.sortBy(_.seenAt)(Ordering[Long].reverse).take(ITEMS)

      // Then, we init accumulator with those
      val accumulator1 = Accumulator(idsToDisplayAnyway)
      val accumulator2 = sortedCandidates.foldLeft(accumulator1) {
        case (acc, info: UsdtInfo) => acc.withUsdtInfo(info)
        case (acc, info: BtcInfo) => acc.withBtcInfo(info)
      }

      // Finally, pick the rest of BTC txs with relation to chosen
      val accumulator3 = btcInfosToConsider.foldLeft(accumulator2) {
        case (acc, info) if acc.identities.intersect(info.relatedTxids).nonEmpty => acc.withBtcInfo(info)
        case (acc, info) if idsToDisplayAnyway.contains(info.txidString) => acc.withBtcInfo(info)
        case (acc, info) if !info.isConfirmed && !info.isDoubleSpent => acc.withBtcInfo(info)
        case (acc, _) => acc
      }

      // Allows reduced list to grow (e.g. 3; got new tx; 4)
      idsToDisplayAnyway ++= accumulator3.identities
      infosFromDb = accumulator3.infos
    }
  }

  def loadSearchedBtcInfos(query: String): Unit = {
    val query1 = query.replaceAll("\\s", "").toLowerCase
    val usdt = WalletApp.usdtTxDataBag.searchTransactions(query1).map(WalletApp.usdtTxDataBag.toInfo)
    val btc = WalletApp.btcTxDataBag.searchTransactions(query1).map(WalletApp.btcTxDataBag.toInfo)
    infosFromDb = usdt ++ btc
  }

  def fillAllInfos: Unit = {
    val pending = WalletApp.pendingInfos.values
    val displayed = pending.toList ++ infosFromDb
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
    val extraInfo: FlowLayout = itemView.findViewById(R.id.extraInfo).asInstanceOf[FlowLayout]
    val description: TextView = itemView.findViewById(R.id.description).asInstanceOf[TextView]
    val statusIcon: ImageView = itemView.findViewById(R.id.statusIcon).asInstanceOf[ImageView]
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

      def proceed: Unit = {
        (Option(extraInput.getText.toString).map(trimmed).filter(_.nonEmpty), currentDetails) match {
          case (labelOpt, info: BtcInfo) => WalletApp.btcTxDataBag.updDescription(info.description.withNewLabel(labelOpt), info.txid)
          case (labelOpt, info: UsdtInfo) => WalletApp.usdtTxDataBag.updDescription(info.description.withNewLabel(labelOpt), info.hashString)
        }
      }
    }

    def shareItem: Unit = currentDetails match {
      case info: BtcInfo => info.description.addresses.headOption.foreach(share)
      case info: UsdtInfo => share(info.description.toAddr)
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

      val sendView = new BtcSendView(specs)
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
          val currentAmount = BtcDenom.directedTT(incoming = receivedMsat, outgoing = 0L.msat, cardOut, cardIn, cardZero, isIncoming = true)
          val afterAmount = BtcDenom.directedTT(feeOpt.map(receivedMsat.-).getOrElse(receivedMsat), 0L.msat, cardOut, cardIn, cardZero, isIncoming = true)
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
                cleanFailedBroadcast(signedTx.txid.toHex, error.message)

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
      val currentFee = BtcDenom.parsedTT(info.feeSat.toMilliSatoshi, cardIn, cardZero)

      val sendView = new BtcSendView(specs)
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
                cleanFailedBroadcast(signedTx.txid.toHex, error.message)

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

      val sendView = new BtcSendView(specs)
      val currentFee = BtcDenom.parsedTT(info.feeSat.toMilliSatoshi, cardIn, cardZero)
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
                cleanFailedBroadcast(signedTx.txid.toHex, error.message)

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

          val txid = getString(popup_txid).format(info.hashString.short0x)
          addFlowChip(extraInfo, txid, R.drawable.border_gray, info.hashString.asSome)

          addFlowChip(extraInfo, getString(dialog_set_label), R.drawable.border_yellow)(setItemLabel)
          if (info.isIncoming) addFlowChip(extraInfo, getString(dialog_share_address), R.drawable.border_yellow)(shareItem)

        case info: BtcInfo =>
          val canRBF = !info.isIncoming && !info.isDoubleSpent && !info.isConfirmed && info.description.cpfpOf.isEmpty
          val canCPFP = info.isIncoming && !info.isDoubleSpent && !info.isConfirmed && info.description.rbf.isEmpty && info.description.canBeCPFPd

          if (ElectrumWallet.specs.size > 1)
            for (wallet <- info.extPubs flatMap ElectrumWallet.specs.get)
              addFlowChip(extraInfo, wallet.info.label, R.drawable.border_gray, None)

          val txid = getString(popup_txid).format(info.txidString.short)
          addFlowChip(extraInfo, txid, R.drawable.border_gray, info.txidString.asSome)

          addFlowChip(extraInfo, getString(dialog_set_label), R.drawable.border_yellow)(setItemLabel)
          if (canRBF) addFlowChip(extraInfo, getString(dialog_boost), R.drawable.border_yellow)(self boostRBF info)
          if (canRBF) addFlowChip(extraInfo, getString(dialog_cancel), R.drawable.border_yellow)(self cancelRBF info)
          if (canCPFP) addFlowChip(extraInfo, getString(dialog_boost), R.drawable.border_yellow)(self boostCPFP info)

          if (info.isIncoming && info.description.addresses.nonEmpty) {
            addFlowChip(extraInfo, getString(dialog_share_address), R.drawable.border_yellow)(shareItem)
          }
      }
    }

    def updateDetails: Unit = {
      setVis(currentDetails.description.label.isDefined, labelIcon)
      if (currentDetails.isDoubleSpent) meta setText getString(state_double_spent).html
      else meta setText WalletApp.app.when(currentDetails.date, WalletApp.app.dateFormat).html

      currentDetails match {
        case info: BtcInfo if info.description.cpfpOf.isDefined => description setText description_cpfp
        case info: BtcInfo if info.description.rbf.exists(_.mode == BtcDescription.RBF_BOOST) => description setText description_rbf_boost
        case info: BtcInfo if info.description.rbf.exists(_.mode == BtcDescription.RBF_CANCEL) => description setText description_rbf_cancel
        case info: UsdtInfo => description setText info.description.label.getOrElse(info.description.toAddr.short0x.html)
        case info: BtcInfo => description setText info.labelOrAddressOpt.getOrElse(me getString tx_btc).html
      }

      currentDetails match {
        case info: BtcInfo if WalletApp.pendingInfos.contains(info.txidString) => itemView.setAlpha(0.6F)
        case info: UsdtInfo if WalletApp.pendingInfos.contains(info.description.fromAddr) => itemView.setAlpha(0.6F)
        case _ => itemView.setAlpha(1F)
      }

      currentDetails match {
        case info: BtcInfo => statusIcon setImageResource btcStatusIcon(info)
        case info: UsdtInfo => statusIcon setImageResource usdtStatusIcon(info)
      }

      currentDetails match {
        case info: BtcInfo if info.description.cpfpOf.isDefined => setVisibleIcon(id = R.id.btcInBoosted)
        case info: BtcInfo if info.description.rbf.exists(_.mode == BtcDescription.RBF_BOOST) => setVisibleIcon(id = R.id.btcOutBoosted)
        case info: BtcInfo if info.description.rbf.exists(_.mode == BtcDescription.RBF_CANCEL) => setVisibleIcon(id = R.id.btcOutCancelled)
        case info: BtcInfo if info.isIncoming => setVisibleIcon(id = R.id.btcIncoming)
        case info: UsdtInfo if info.isIncoming => setVisibleIcon(R.id.usdtIncoming)
        case _: UsdtInfo => setVisibleIcon(id = R.id.usdtOutgoing)
        case _: BtcInfo => setVisibleIcon(id = R.id.btcOutgoing)
      }

      currentDetails match {
        case info: BtcInfo => amount.setText(BtcDenom.directedTT(info.receivedSat.toMilliSatoshi, info.sentSat.toMilliSatoshi, cardOut, cardIn, cardZero, info.isIncoming).html)
        case info: UsdtInfo => amount.setText(Denomination.fiatDirectedTT(info.receivedUsdtString, info.sentUsdtString, cardOut, cardIn, info.isIncoming).html)
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
      val in = WalletApp.linkUsdt.data.okWallets.get(info.description.toAddr)
      val out = WalletApp.linkUsdt.data.okWallets.get(info.description.fromAddr)
      val isDeeplyBuried = in.exists(_.chainTip - info.block >= 20) || out.exists(_.chainTip - info.block >= 20)
      val isUnknown = in.isEmpty && out.isEmpty

      if (info.isDoubleSpent && isDeeplyBuried) R.drawable.block_24
      else if (info.isDoubleSpent || isUnknown) R.drawable.question_24
      else if (isDeeplyBuried) R.drawable.done_24
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
    val recentActivity: View = view.findViewById(R.id.recentActivity)
    val manager: WalletCardManager = new WalletCardManager(holder)
    // This means search is off at start
    searchField.setTag(false)

    def makeCards: List[WalletCard] = {
      val btcCards = for (xPub <- ElectrumWallet.specs.keys) yield new BtcWalletCard(me, xPub) {
        override def onWalletTap: Unit = goToWithValue(ClassNames.qrBtcActivityClass, xPub)
      }

      val usdtCards = for (info <- WalletApp.linkUsdt.data.wallets) yield new UsdtWalletCard(me, info.xPriv) {
        override def onWalletTap: Unit = WalletApp.linkUsdt.data.withRealAddress.find(_.xPriv == xPriv) match {
          case Some(info) => goToWithValue(ClassNames.qrUsdtActivityClass, info)
          case None => WalletApp.app.quickToast(usdt_not_ready)
        }
      }

      val taCardOpt = if (WalletApp.showTaCard) new TaWalletCard(me) {
        override def onWalletTap: Unit = goTo(ClassNames.taActivityClass)
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
    override def onWalletReady(event: WalletReady): Unit =
      DbStreams.next(DbStreams.txDbStream)
  }

  private val usdtListener = new LinkUsdt.Listener(LinkUsdt.GENERAL_ERROR) {
    override def onChainTip(chainTip: Long): Unit = paymentAdapterDataChanged.run
    override def onResponse(args: Option[LinkUsdt.ResponseArguments] = None): Unit = args.foreach {
      case failure: LinkUsdt.UsdtFailure => WalletApp.app.quickToast(failure.failureCode.getClass.getName)
      case _ => // Not interested in anything else
    }
  }

  private val fiatListener = new FiatRatesListener {
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
    val dataOpt = Seq(getIntent.getDataString, getIntent getStringExtra Intent.EXTRA_TEXT).find(externalData => null != externalData)
    runInFutureProcessOnUI(dataOpt foreach InputParser.recordValue, none)(_ => try checkExternalData(noneRunnable) catch none)
    setIntent(new Intent)
  }

  override def onDestroy: Unit = {
    try ElectrumWallet.catcher ! WalletEventsCatcher.Remove(chainListener) catch none
    try WalletApp.linkUsdt ! LinkUsdt.CmdRemove(usdtListener) catch none
    try WalletApp.fiatRates.listeners -= fiatListener catch none
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

    def bringSingleAddressSelector(bitcoinUri: BitcoinUri) = new BtcWalletSelector(me titleViewFromUri bitcoinUri) {
      def onOk: Unit = bringSendBitcoinPopup(chosenCards.toList, bitcoinUri)
    }

    def bringMultiAddressSelector(a2a: MultiAddressParser.AddressToAmount) = new BtcWalletSelector(me getString dialog_send_btc_many) {
      def onOk: Unit = bringSendMultiBitcoinPopup(chosenCards.toList, a2a)
    }

    InputParser.checkAndMaybeErase {
      case bitcoinUri: BitcoinUri if Try(ElectrumWallet addressToPubKeyScript bitcoinUri.address).isSuccess =>
        if (spendable.size == 1) bringSendBitcoinPopup(spendable, bitcoinUri)
        else if (usable.size == 1) bringSendBitcoinPopup(usable, bitcoinUri)
        else bringSingleAddressSelector(bitcoinUri)

      case a2a: MultiAddressParser.AddressToAmount if a2a.values.nonEmpty =>
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
        runInFutureProcessOnUI(walletAddresses.find(addressInfo => data.address == addressInfo.address), onFail) {
          case Some(info) => bringSignDialog(getString(sign_sign_message_notx_title).format(info.address.short).asDefView, info).setText(data.message)
          case None => WalletApp.app.quickToast(sign_address_not_found)
        }

      case data: String if toChecksumAddress(data) == data =>
        bringSendUsdtPopup(data)

      case _ =>
        whenNone.run
    }
  }

  override def onBackPressed: Unit = {
    if (isSearchOn) rmSearch(view = null)
    else if (displayFullIxInfoHistory) {
      displayFullIxInfoHistory = false
      allInfos = allInfos.take(ITEMS)
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

        ElectrumWallet.catcher ! chainListener
        WalletApp.fiatRates.listeners += fiatListener
        WalletApp.linkUsdt ! usdtListener

        itemsList.addHeaderView(walletCards.view)
        itemsList.addFooterView(expandContainer)
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

        stateSubscription = Rx.uniqueFirstAndLastWithinWindow(DbStreams.txDbStream, 500.millis).subscribe { _ =>
          // This gets triggered whenever it makes sense to load new infos from db (info added, broadcasted, description changed)
          // For BTC txs specifically, we also leverage this opportunity to check whether they got confirmed or double-spent
          if (!isSearchOn) loadRecent

          for {
            info <- btcInfosToConsider if !info.isDoubleSpent && !info.isConfirmed
            relatedSpec <- info.extPubs.flatMap(ElectrumWallet.specs.get).headOption
            doubleSpent = ElectrumWallet.doubleSpent(relatedSpec.data.keys.ewt.xPub, info.tx)
            if doubleSpent.depth != info.depth || doubleSpent.isDoubleSpent != info.isDoubleSpent
          } WalletApp.btcTxDataBag.updStatus(info.txid, doubleSpent.depth, doubleSpent.stamp, doubleSpent.isDoubleSpent)

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
  }

  def rmSearch(view: View): Unit = {
    walletCards.searchField.setTag(false)
    walletCards.searchField.setText(new String)
    androidx.transition.TransitionManager.beginDelayedTransition(contentWindow)
    setVisMany(true -> walletCards.defaultHeader, false -> walletCards.searchField)
  }

  def bringPasteAddressDialog: Unit = {
    def doBringPasteAddressDialog: Unit = {
      val (builder, extraInputLayout, extraInput) = singleInputPopupBuilder
      mkCheckForm(alert => runAnd(alert.dismiss)(proceed), none, builder, dialog_ok, dialog_cancel)
      extraInputLayout.setHint(typing_hints)

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

  def bringSendUsdtPopup(address: String): Unit = {
    val sendView = new UsdtSendView
    val title = new TitleView(getString(dialog_send_usdt) format address.short0x)
    val builder = titleBodyAsViewBuilder(title.asColoredView(R.color.usdt), sendView.body)
    mkCheckForm(identity, none, builder, dialog_ok, dialog_cancel)
  }

  def bringSendBitcoinPopup(specs: Seq[WalletSpec], uri: BitcoinUri): Unit = {
    val pubKeyScript = ElectrumWallet.addressToPubKeyScript(uri.address)
    val changeTo = ElectrumWallet.orderByImportance(specs).head
    val sendView = new BtcSendView(specs)

    def attempt(alert: AlertDialog): Unit =
      runInFutureProcessOnUI(ElectrumWallet.makeTx(specs, changeTo, pubKeyScript, sendView.manager.resultMsat.truncateToSatoshi, Map.empty, feeView.rate), onFail) { response =>
        // This may be a signing or a hardware wallet, in case if it's a hardware wallet we need additional UI action so we use this proxy method here

        proceedConfirm(sendView, alert, response) { signedTx =>
          val desc = PlainBtcDescription(uri.address :: Nil, uri.label orElse uri.message)
          val broadcastFuture = broadcastTx(desc, signedTx, received = Satoshi(0L), sent = response.transferred, response.fee, incoming = 0)
          runFutureProcessOnUI(broadcastFuture, onFail) { case Some(error) => cleanFailedBroadcast(signedTx.txid.toHex, error.message) case None => }
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

    lazy val feeView = new FeeView[GenerateTxResponse](FeeratePerByte(1L.sat), sendView.defaultView.host) {
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
    val changeTo = ElectrumWallet.orderByImportance(specs).head
    val sendView = new BtcSendView(specs)

    def attempt(alert: AlertDialog): Unit =
      runInFutureProcessOnUI(ElectrumWallet.makeBatchTx(specs, changeTo, scriptToAmount, feeView.rate), onFail) { response =>
        // This may be a signing or a hardware wallet, in case if it's a hardware wallet we need additional UI action so we use this method here

        proceedConfirm(sendView, alert, response) { signedTx =>
          val desc = PlainBtcDescription(addressToAmount.values.firstItems.toList)
          val broadcastFuture = broadcastTx(desc, signedTx, received = Satoshi(0L), sent = response.transferred, response.fee, incoming = 0)
          runFutureProcessOnUI(broadcastFuture, onFail) { case Some(error) => cleanFailedBroadcast(signedTx.txid.toHex, error.message) case None => }
          alert.dismiss
        }
      }

    lazy val alert = {
      val view = new TitleView(me getString dialog_send_btc_many).asColoredView(R.color.cardBitcoinSigning)
      mkCheckForm(attempt, none, titleBodyAsViewBuilder(view, sendView.body), dialog_ok, dialog_cancel)
    }

    lazy val feeView = new FeeView[GenerateTxResponse](FeeratePerByte(1L.sat), sendView.defaultView.host) {
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
      val humanAmount = BtcDenom.parsedTT(amount.toMilliSatoshi, cardIn, cardZero)
      val parent = getLayoutInflater.inflate(R.layout.frag_two_sided_item, null)
      new TwoSidedItem(parent, address.short.html, humanAmount.html)
      sendView.editChain.addView(parent, 0)
    }

    setVis(isVisible = false, sendView.inputChain)
    feeView.update(feeOpt = None, showIssue = false)
    feeView.worker addWork "MULTI-SEND-INIT-CALL"
  }

  def bringSignDialog(title: LinearLayout, info: BtcAddressAndPrivKey): EditText = {
    val (container, extraInputLayout, extraInputField, extraOption, extraOptionText) = singleInputPopup
    mkCheckForm(alert => runAnd(alert.dismiss)(proceed), none, titleBodyAsViewBuilder(title, container), sign_sign, dialog_cancel)

    def proceed: Unit = {
      val message = extraInputField.getText.toString.trim
      val hash = drongo.crypto.Bip322.getBip322MessageHash(message)
      val msgOpt = if (extraOption.isChecked) None else Some(message)
      val signingKey = drongo.crypto.ECKey.fromPrivate(info.privKey.toArray)
      val scriptType = drongo.address.Address.fromString(drongoNetwork, info.address)
      val sig = drongo.crypto.Bip322.signMessageBip322(scriptType.getScriptType, message, signingKey)
      val data = BIP322VerifyData(info.address, ByteVector.view(hash), sig, msgOpt)
      goToWithValue(ClassNames.qrSigActivityClass, data)
    }

    extraInputLayout.setHint(sign_message)
    setVisMany(true -> extraOptionText, true -> extraOption)
    extraOptionText.setText(sign_sig_only_info)
    extraOption.setText(sign_sig_only)
    extraInputField
  }

  def paymentAdapterDataChanged: TimerTask = UITask {
    // Do not show an exand button IF user has chosen to show full history OR searching is on OR we have nothing extra to show
    val expandHideCases = displayFullIxInfoHistory || isSearchOn || btcInfosToConsider.size + usdtInfosToConsider.size <= allInfos.size
    setVisMany(allInfos.nonEmpty -> walletCards.recentActivity, !expandHideCases -> expandContainer)
    paymentsAdapter.notifyDataSetChanged
  }

  def proceedConfirm(sendView: BtcSendView, alert: AlertDialog, response: GenerateTxResponse)(process: Transaction => Unit): Unit = {
    sendView.confirmView.chainButtonsView.chainNextButton setOnClickListener onButtonTap(process apply response.tx)
    sendView.switchToConfirm(alert, response)
  }

  def proceedWithoutConfirm(alert: AlertDialog, response: GenerateTxResponse)(process: Transaction => Unit): Unit = {
    process(response.tx)
    alert.dismiss
  }

  def broadcastTx(desc: BtcDescription, finalTx: Transaction, received: Satoshi, sent: Satoshi, fee: Satoshi, incoming: Int): Future[OkOrError] = {
    val info = BtcInfo(finalTx.toString, finalTx.txid.toHex, invalidPubKey.toString, depth = 0, received, sent, fee, seenAt = System.currentTimeMillis,
      updatedAt = System.currentTimeMillis, desc, 0L.msat, WalletApp.fiatRates.info.rates.toJson.compactPrint, incoming, doubleSpent = 0)
    WalletApp.pendingInfos(info.txidString) = info
    WalletApp.seenInfos(info.txidString) = info
    DbStreams.next(DbStreams.txDbStream)
    ElectrumWallet.broadcast(finalTx)
  }

  def cleanFailedBroadcast(failedKey: String, message: String): Unit = {
    WalletApp.pendingInfos.remove(failedKey)
    WalletApp.seenInfos.remove(failedKey)
    DbStreams.next(DbStreams.txDbStream)
    onFail(message)
  }

  def walletAddresses = for {
    keys <- ElectrumWallet.specs.values.map(_.data.keys)
    (_, extPubKey) <- keys.accountKeyMap ++ keys.changeKeyMap
    privKey = keys.ewt.extPrivKeyFromPub(extPubKey).privateKey.value
  } yield BtcAddressAndPrivKey(keys.ewt.textAddress(extPubKey), privKey)

  def drongoNetwork = {
    ElectrumWallet.chainHash match {
      case Block.LivenetGenesisBlock.hash => drongo.Network.MAINNET
      case Block.TestnetGenesisBlock.hash => drongo.Network.TESTNET
      case _ => drongo.Network.REGTEST
    }
  }
}