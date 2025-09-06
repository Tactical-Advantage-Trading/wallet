package trading.tacticaladvantage

import android.content.Intent
import android.content.pm.PackageManager
import android.os.Bundle
import android.util.Patterns
import android.view.{View, ViewGroup}
import android.widget._
import androidx.appcompat.app.AlertDialog
import androidx.recyclerview.widget.RecyclerView
import com.sparrowwallet.drongo
import fr.acinq.bitcoin._
import fr.acinq.eclair._
import fr.acinq.eclair.blockchain.electrum.ElectrumWallet._
import fr.acinq.eclair.blockchain.electrum.{ElectrumWallet, WalletSpec}
import fr.acinq.eclair.blockchain.fee.FeeratePerByte
import immortan.Tools._
import immortan._
import immortan.sqlite.{CompleteUsdtWalletInfo, DbStreams}
import immortan.utils.ImplicitJsonFormats._
import immortan.utils._
import org.apmem.tools.layouts.FlowLayout
import org.web3j.crypto.Keys.toChecksumAddress
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

class MainActivity extends BaseActivity with MnemonicActivity with ExternalDataChecker { me =>
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
    infosFromDb = usdtInfosToConsider ++ btcInfosToConsider

    if (!displayFullIxInfoHistory) {
      // First, select a number of most recent chronological items to display by default
      val sortedCandidates = infosFromDb.toList.sortBy(_.seenAt)(Ordering[Long].reverse).take(ITEMS)

      // Then, we init accumulator with those
      val accumulator1 = Accumulator(idsToDisplayAnyway)
      val accumulator2 = sortedCandidates.foldLeft(accumulator1) {
        case (acc, info: UsdtInfo) => acc.withUsdtInfo(info)
        case (acc, info: BtcInfo) => acc.withBtcInfo(info)
      }

      // Then see if any dropped ones should be displayed
      val accumulator3 = infosFromDb.foldLeft(accumulator2) {
        case (acc, info: UsdtInfo) if idsToDisplayAnyway.contains(info.identity) => acc.withUsdtInfo(info)
        case (acc, info: BtcInfo) if acc.identities.intersect(info.relatedTxids).nonEmpty => acc.withBtcInfo(info)
        case (acc, info: BtcInfo) if idsToDisplayAnyway.contains(info.identity) => acc.withBtcInfo(info)
        case (acc, info: BtcInfo) if !info.isConfirmed && !info.isDoubleSpent => acc.withBtcInfo(info)
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
      setVis(item.isExpandedItem, holder.spacer)
      holder.currentDetails = item
      holder.updateDetails
      view
    }
  }

  class PaymentLineViewHolder(itemView: View) extends RecyclerView.ViewHolder(itemView) { self =>
    val extraInfo: FlowLayout = itemView.findViewById(R.id.extraInfo).asInstanceOf[FlowLayout]
    val statusIcon: ImageView = itemView.findViewById(R.id.statusIcon).asInstanceOf[ImageView]
    val amount: TextView = itemView.findViewById(R.id.amount).asInstanceOf[TextView]
    val meta: TextView = itemView.findViewById(R.id.meta).asInstanceOf[TextView]
    val spacer: View = itemView.findViewById(R.id.spacer)
    itemView.setTag(this)

    val paymentCardContainer: View = itemView.findViewById(R.id.paymentCardContainer)
    val paymentTypeIconViews: List[View] = paymentTypeIconIds.map(itemView.findViewById)
    val iconMap: Map[Int, View] = paymentTypeIconIds.zip(paymentTypeIconViews).toMap
    var currentDetails: ItemDetails = _
    var lastVisibleIconId: Int = -1

    paymentCardContainer setOnClickListener onButtonTap {
      val isVisible = extraInfo.getVisibility == View.VISIBLE
      if (isVisible) collapse(currentDetails) else expand(currentDetails)
    }

    // CPFP / RBF

    def boostCPFP(info: BtcInfo): Unit = info.extPubs.flatMap(ElectrumWallet.specs.get) match {
      case Nil => WalletApp.app.quickToast(error_no_wallet)
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
      lazy val feeView = new FeeView[GenerateTxResponse](FeeratePerByte(target), sendView.cpfpView.fvc) {
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
          updatePosButton(alert, feeOpt.isDefined).run
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
            runFutureProcessOnUI(broadcastBtc(desc, signedTx, response.transferred, sent = Satoshi(0L), response.fee, incoming = 1), onFail) {

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
      feeView.fvc.customFeerateOption.performClick
      sendView.defaultView = sendView.cpfpView
      sendView.switchToDefault(alert)
    }

    def boostRBF(info: BtcInfo): Unit = info.extPubs.flatMap(ElectrumWallet.specs.get) match {
      case res if res.size < info.extPubs.size => WalletApp.app.quickToast(error_no_wallet)
      case specs => doBoostRBF(specs, info)
    }

    def doBoostRBF(specs: Seq[WalletSpec], info: BtcInfo): Unit = {
      val changeTo = ElectrumWallet.orderByImportance(candidates = specs).head
      val currentFee = BtcDenom.parsedTT(info.feeSat.toMilliSatoshi, cardIn, cardZero)

      val sendView = new BtcSendView(specs)
      val blockTarget = WalletApp.feeRates.info.onChainFeeConf.feeTargets.fundingBlockTarget
      val target = WalletApp.feeRates.info.onChainFeeConf.feeEstimator.getFeeratePerKw(blockTarget)
      lazy val feeView = new FeeView[RBFResponse](FeeratePerByte(target), sendView.rbfView.fvc) {
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
          super.update(feeOpt = Option.empty, showIssue = false)
          setVis(isVisible = true, sendView.rbfView.rbfIssue)
          updatePosButton(alert, isEnabled = false).run
          sendView.rbfView.rbfIssue.setText(descRes)
        }.run

        override def update(feeOpt: Option[MilliSatoshi], showIssue: Boolean): Unit = UITask {
          updatePosButton(alert, isEnabled = feeOpt.isDefined).run
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
            runFutureProcessOnUI(broadcastBtc(desc, signedTx, received = Satoshi(0L), info.sentSat, response.fee, incoming = 0), onFail) {

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
      feeView.fvc.customFeerateOption.performClick
      sendView.defaultView = sendView.rbfView
      sendView.switchToDefault(alert)
    }

    def cancelRBF(info: BtcInfo): Unit = info.extPubs.flatMap(ElectrumWallet.specs.get) match {
      case Nil => WalletApp.app.quickToast(error_no_wallet)
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
      lazy val feeView = new FeeView[RBFResponse](FeeratePerByte(target), sendView.rbfView.fvc) {
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
          super.update(feeOpt = Option.empty, showIssue = false)
          setVis(isVisible = true, sendView.rbfView.rbfIssue)
          updatePosButton(alert, isEnabled = false).run
          sendView.rbfView.rbfIssue.setText(descRes)
        }.run

        override def update(feeOpt: Option[MilliSatoshi], showIssue: Boolean): Unit = UITask {
          updatePosButton(alert, isEnabled = feeOpt.isDefined).run
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
            runFutureProcessOnUI(broadcastBtc(desc, signedTx, info.sentSat - response.fee, sent = 0L.sat, response.fee, incoming = 1), onFail) {

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
      feeView.fvc.customFeerateOption.performClick
      sendView.defaultView = sendView.rbfView
      sendView.switchToDefault(alert)
    }

    // VIEW RELATED

    def collapse[T <: ItemDetails](item: T): Unit = {
      setVis(isVisible = false, extraInfo)
      extraInfo.removeAllViewsInLayout
      openListItems -= item.identity
    }

    def expand[T <: ItemDetails](item: T): Unit = {
      setVis(isVisible = true, extraInfo)
      extraInfo.removeAllViewsInLayout
      openListItems += item.identity

      item match {
        case info: UsdtInfo =>
          val hash = getString(popup_txid).format(info.identity.short0x)
          addFlowChip(extraInfo, hash, R.drawable.border_gray, info.identity.asSome)

        case info: BtcInfo =>
          val canRBF = !info.isIncoming && !info.isDoubleSpent && !info.isConfirmed && info.description.cpfpOf.isEmpty
          val canCPFP = info.isIncoming && !info.isDoubleSpent && !info.isConfirmed && info.description.rbf.isEmpty && info.description.canBeCPFPd

          val txid = getString(popup_txid).format(info.identity.short)
          addFlowChip(extraInfo, txid, R.drawable.border_gray, info.identity.asSome)
          if (canRBF) addFlowChip(extraInfo, getString(dialog_boost), R.drawable.border_yellow)(self boostRBF info)
          if (canRBF) addFlowChip(extraInfo, getString(dialog_cancel), R.drawable.border_yellow)(self cancelRBF info)
          if (canCPFP) addFlowChip(extraInfo, getString(dialog_boost), R.drawable.border_yellow)(self boostCPFP info)
      }
    }

    def updateDetails: Unit = {
      meta setText WalletApp.app.dateFormat.format(currentDetails.date).html

      currentDetails match {
        case info: BtcInfo if WalletApp.pendingInfos.contains(info.identity) => itemView.setAlpha(0.6F)
        case info: UsdtInfo if info.identity == CompleteUsdtWalletInfo.NOIDENTITY => itemView.setAlpha(0.6F)
        case _ if currentDetails.isDoubleSpent => itemView.setAlpha(0.6F)
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
        case info: BtcInfo if info.description.cpfpOf.isDefined => amount.setText(description_cpfp)
        case info: BtcInfo if info.description.rbf.exists(_.mode == BtcDescription.RBF_BOOST) => amount.setText(description_rbf_boost)
        case info: BtcInfo if info.description.rbf.exists(_.mode == BtcDescription.RBF_CANCEL) => amount.setText(description_rbf_cancel)
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
      val isDeeplyBuried = (in ++ out).exists(_.chainTip - info.block >= 20)
      val isUnknown = info.identity == CompleteUsdtWalletInfo.NOIDENTITY
      val isAlien = in.isEmpty && out.isEmpty

      if (info.isDoubleSpent && isDeeplyBuried) R.drawable.block_24
      else if (info.isDoubleSpent || isUnknown || isAlien) R.drawable.question_24
      else if (isDeeplyBuried) R.drawable.done_24
      else R.drawable.hourglass_empty_24
    }
  }

  // LIST CAPTION CLASS

  class WalletCardsViewHolder {
    val view = getLayoutInflater.inflate(R.layout.frag_wallet_cards, null).asInstanceOf[LinearLayout]
    val fiatUnitPriceAndChange = view.findViewById(R.id.fiatUnitPriceAndChange).asInstanceOf[TextView]
    val defaultHeader = view.findViewById(R.id.defaultHeader).asInstanceOf[LinearLayout]
    val holder = view.findViewById(R.id.chainCardsContainer).asInstanceOf[LinearLayout]
    val recentActivity = view.findViewById(R.id.recentActivity).asInstanceOf[View]
    val searchField = view.findViewById(R.id.searchField).asInstanceOf[EditText]
    val manager = new WalletCardManager(holder)

    // Settings region
    val settingsContainer = view.findViewById(R.id.settingsContainer).asInstanceOf[LinearLayout]
    val devInfo = me clickableTextField settingsContainer.findViewById(R.id.devInfo).asInstanceOf[TextView]
    val settingsButtons = settingsContainer.findViewById(R.id.settingsButtons).asInstanceOf[FlowLayout]
    val nameAndVer = settingsContainer.findViewById(R.id.nameAndVer).asInstanceOf[TextView]
    val appName = s"${me getString app_name} <font color=$cardZero>v1</font>"
    val btc = 100000000000L.msat

    devInfo.setText(getString(dev_info).html)
    nameAndVer.setText(appName.html)
    searchField.setTag(false)

    def showBtcWalletCard = {
      val nativeSpec = WalletApp.createBtcWallet(WalletApp.secret)
      WalletApp.postInitBtcWallet(spec = nativeSpec)
    }

    def showUsdtWalletCard = {
      WalletApp.linkUsdt ! WalletApp.createUsdtWallet(WalletApp.secret)
      WalletApp.linkUsdt ! LinkUsdt.CmdEnsureUsdtAccounts
    }

    def attachBtcWallet = showMnemonicInput(action_recovery_phrase_title) { mnemonic =>
      val attachedKeys = MasterKeys.fromSeed(MnemonicCode.toSeed(mnemonic, new String).toArray)
      WalletApp.attachBtcWallet(attachedKeys)
    }

    def makeCards = {
      lazy val taClientCard = new TaWalletCard(host = me) {
        override def hide: Unit = WalletApp.setTaCard(visible = false)
        override def onTap: Unit = WalletApp.linkClient.data match {
          case _: LinkClient.UserStatus if earnAccount.isExpanded =>
            androidx.transition.TransitionManager.beginDelayedTransition(defaultHeader)
            setVisMany(false -> earnAccount.wrap, true -> infoContainer)
          case _: LinkClient.UserStatus =>
            androidx.transition.TransitionManager.beginDelayedTransition(defaultHeader)
            setVisMany(true -> earnAccount.wrap, false -> infoContainer)
          case LinkClient.LoggedOut =>
            bringTaSignInDialogEmail
        }
      }

      val btcCards =
        for (xPub <- ElectrumWallet.specs.keys)
          yield new BtcWalletCard(host = me, xPub) {
            override def hide: Unit = WalletApp.removeBtcWallet(xPub)
            override def onTap: Unit = goToWithValue(ClassNames.qrBtcActivityClass, xPub)
          }

      val usdtCards =
        for (info <- WalletApp.linkUsdt.data.wallets)
          yield new UsdtWalletCard(host = me, info.xPriv) {
            override def hide: Unit = WalletApp.linkUsdt ! LinkUsdt.CmdRemoveWallet(info.xPriv)
            override def onTap: Unit = WalletApp.linkUsdt.data.withRealAddress.find(_.xPriv == xPriv) match {
              case Some(info) => goToWithValue(ClassNames.qrUsdtActivityClass, info)
              case None => WalletApp.app.quickToast(usdt_not_ready)
            }
          }

      val wallets = btcCards.toList ++ usdtCards
      if (WalletApp.showTaCard) wallets :+ taClientCard
      else wallets
    }

    def resetCards: Unit = {
      holder.removeAllViewsInLayout
      manager.init(makeCards)
      updateView
    }

    def updateView: Unit = {
      androidx.transition.TransitionManager.beginDelayedTransition(defaultHeader)
      val change = WalletApp.fiatRates.info.pctDifference(code = WalletApp.fiatCode).getOrElse(default = new String)
      val unitRate = WalletApp.msatInFiatHuman(WalletApp.fiatRates.info.rates, WalletApp.fiatCode, btc, Denomination.formatFiatShort)
      fiatUnitPriceAndChange.setText(s"₿ ≈ $unitRate $change".html)
      manager.cardViews.foreach(_.updateView)

      settingsButtons.removeAllViewsInLayout
      setVis(isVisible = isSettingsOn, view = settingsButtons)
      for (view <- walletCards.manager.cardViews) setVis(isSettingsOn, view.cardButtons)

      if (isSettingsOn) {
        if (ElectrumWallet.specs.values.count(_.info.core.attachedMaster.isEmpty) < 1) addFlowChip(settingsButtons, getString(settings_show_btc), R.drawable.border_yellow)(showBtcWalletCard)
        if (WalletApp.linkUsdt.data.wallets.isEmpty) addFlowChip(settingsButtons, getString(settings_show_usdt), R.drawable.border_yellow)(showUsdtWalletCard)
        if (!WalletApp.showTaCard) addFlowChip(settingsButtons, getString(settings_show_ta), R.drawable.border_yellow)(WalletApp setTaCard true)
        addFlowChip(settingsButtons, getString(settings_view_recovery_phrase), R.drawable.border_blue)(viewRecoveryCode)
        addFlowChip(settingsButtons, getString(settings_attach_btc_wallet), R.drawable.border_blue)(attachBtcWallet)
      }
    }
  }

  // LISTENERS

  private var viewUpdateSub = Option.empty[Subscription]
  private var cardsResetSub = Option.empty[Subscription]

  private val btcChainListener = new WalletEventsListener {
    override def onWalletReady(event: WalletReady): Unit =
      DbStreams.next(DbStreams.txStream)
  }

  private val usdtListener = new LinkUsdt.Listener(LinkUsdt.GENERAL_ERROR) {
    override def onChainTip(chainTip: Long): Unit = paymentAdapterDataChanged.run
    override def onResponse(args: Option[LinkUsdt.ResponseArguments] = None): Unit = args.foreach {
      case fail: LinkUsdt.UsdtFailure => UITask(WalletApp.app quickToast fail.failureCode.toString).run
      case _ => // Not interested in anything else
    }
  }

  private val taErrorListener = new LinkClient.Listener(LinkClient.GENERAL_ERROR) {
    override def onResponse(args: Option[LinkClient.ResponseArguments] = None): Unit = args.foreach {
      case fail: LinkClient.Failure => UITask(WalletApp.app quickToast fail.failureCode.toString).run
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
    try ElectrumWallet.catcher ! WalletEventsCatcher.Remove(btcChainListener) catch none
    try WalletApp.linkClient ! LinkClient.CmdRemove(taErrorListener) catch none
    try WalletApp.linkUsdt ! LinkUsdt.CmdRemove(usdtListener) catch none
    try WalletApp.fiatRates.listeners -= fiatListener catch none
    viewUpdateSub.foreach(_.unsubscribe)
    cardsResetSub.foreach(_.unsubscribe)
    super.onDestroy
  }

  override def onRequestPermissionsResult(reqCode: Int, permissions: Array[String], results: Array[Int] = Array.empty): Unit =
    if (reqCode == scannerRequestCode && results.nonEmpty && results.head == PackageManager.PERMISSION_GRANTED) bringScanner(null)

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
        WalletApp.linkUsdt.data.withRealAddress.headOption match {
          case Some(info) => bringSendUsdtPopup(new UsdtSendView(info), data)
          case None => WalletApp.app.quickToast(error_no_wallet)
        }

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

  def isSearchOn: Boolean = walletCards.searchField.getTag.asInstanceOf[Boolean]
  def isSettingsOn: Boolean = walletCards.settingsContainer.getVisibility == View.VISIBLE

  override def START(state: Bundle): Unit =
    WalletApp.isAlive match {
      case true if WalletApp.isOperational =>
        setContentView(R.layout.activity_main)
        ElectrumWallet.catcher ! btcChainListener
        WalletApp.fiatRates.listeners += fiatListener
        WalletApp.linkClient ! taErrorListener
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
        runAnd { loadRecent } { paymentAdapterDataChanged.run }
        walletCards.searchField addTextChangedListener onTextChange(searchWorker.addWork)

        // STREAMS

        viewUpdateSub = Rx.uniqueFirstAndLastWithinWindow(DbStreams.txStream, 500.millis) {
          // Full view update when it makes sense to load new infos from db (info added, broadcasted, description changed)
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

        cardsResetSub = Rx.uniqueFirstAndLastWithinWindow(DbStreams.walletStream, 500.millis) {
          // Full wallet cards reset when it makes sense to reload them from db
          UITask(walletCards.resetCards).run
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
      val (builder, extraInputLayout, extraInput) = singleInputPopupBuilder(title = null)
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

  def toggleSettingsMode(view: View): Unit = {
    setVis(!isSettingsOn, walletCards.settingsContainer)
    walletCards.updateView
  }

  def bringSendUsdtPopup(sendView: UsdtSendView, address: String): Unit = {
    val title = new TitleView(getString(dialog_send_usdt) format address.short0x)
    val builder = titleBodyAsViewBuilder(title.asColoredView(R.color.usdt), sendView.body)
    var fee = BigDecimal(-1)

    def reactOnInput(unused: String): BigDecimal = {
      val value = BigDecimal(sendView.editView.rmc.fiatInputAmount.getNumericValueBigDecimal)
      val canProceed = value >= CompleteUsdtWalletInfo.DUST_THRESHOLD && value <= sendView.info.lastBalanceDecimal
      updatePosButton(alert, isEnabled = fee >= 0 && canProceed).run
      value
    }

    def attempt(alert: AlertDialog): Unit = {
      val inputAmount = reactOnInput(unused = null)
      val finalAmount = if (inputAmount + fee > sendView.info.lastBalanceDecimal) sendView.info.lastBalanceDecimal - fee else inputAmount
      sendView.confirmView.confirmAmount.secondItem setText Denomination.fiatTT("0", finalAmount.toString, null, cardIn, isIncoming = false).html
      sendView.confirmView.chainEditButton setOnClickListener onButtonTap(sendView switchToDefault alert)
      sendView.confirmView.chainCancelButton setOnClickListener onButtonTap(alert.dismiss)
      sendView.switchButtons(alert, on = false)
      sendView.switchTo(sendView.confirmView)

      sendView.confirmView.chainNextButton setOnClickListener onButtonTap {
        val desc = UsdtDescription(fromAddr = sendView.info.lcAddress, toAddr = address)
        val req = Biconomy.TxDetailsRequest(sendView.info.xPriv, recipient = address, Biconomy.USDt, amount = (BigDecimal(10).pow(6) * finalAmount).toBigInt.toString)
        runFutureProcessOnUI(broadcastUsdt(desc, req, finalAmount, fee), onFail) { case None => cleanFailedBroadcast(desc.fromAddr, "USD₮ transfer failed") case _ => }
        alert.dismiss
      }
    }

    def useMax(unused: AlertDialog): Unit = sendView.editView.rmc.updateFiatText(sendView.info.lastBalanceDecimal.toString)
    lazy val alert = mkCheckFormNeutral(attempt, none, useMax, builder, dialog_ok, dialog_cancel, dialog_max)
    sendView.editView.rmc.fiatInputAmount addTextChangedListener onTextChange(reactOnInput)
    updatePosButton(alert, isEnabled = false).run

    val req = Biconomy.TxDetailsRequest(sendView.info.xPriv, address, Biconomy.USDt)
    runInFutureProcessOnUI(WalletApp.biconomy.estimateTxGas(req), onFail) {
      case Some(response) =>
        fee = BigDecimal(response.feeAmount).max(fee) + CompleteUsdtWalletInfo.DUST_THRESHOLD
        val humanFee = Denomination.fiatTT("0", fee.toString, null, cardIn, isIncoming = false)
        sendView.editView.fvc.feeRate setText getString(dialog_fee).format(humanFee).html
        sendView.confirmView.confirmFee.secondItem setText humanFee.html
        reactOnInput(unused = null)
      case None =>
        val errorMsg = s"<font color=$cardZero>${me getString dialog_fee_error}</font>"
        sendView.editView.fvc.feeRate setText getString(dialog_fee).format(errorMsg).html
    }
  }

  def bringSendBitcoinPopup(specs: Seq[WalletSpec], uri: BitcoinUri): Unit = {
    val pubKeyScript = ElectrumWallet.addressToPubKeyScript(uri.address)
    val changeTo = ElectrumWallet.orderByImportance(specs).head
    val sendView = new BtcSendView(specs)

    def attempt(alert: AlertDialog): Unit =
      runInFutureProcessOnUI(ElectrumWallet.makeTx(specs, changeTo, pubKeyScript, sendView.rm.resultMsat.truncateToSatoshi, Map.empty, feeView.rate), onFail) { response =>
        // This may be a signing or a hardware wallet, in case if it's a hardware wallet we need additional UI action so we use this proxy method here

        proceedConfirm(sendView, alert, response) { signedTx =>
          val desc = PlainBtcDescription(uri.address :: Nil, uri.label orElse uri.message)
          val broadcastFuture = broadcastBtc(desc, signedTx, received = Satoshi(0L), sent = response.transferred, response.fee, incoming = 0)
          runFutureProcessOnUI(broadcastFuture, onFail) { case Some(error) => cleanFailedBroadcast(signedTx.txid.toHex, error.message) case None => }
          alert.dismiss
        }
      }

    def useMax(unused: AlertDialog): Unit = {
      val totalBalance = specs.map(_.info.lastBalance).sum
      sendView.rm.updateText(totalBalance.toMilliSatoshi)
    }

    lazy val alert = {
      val title = titleViewFromUri(uri)
      val neutralRes = if (uri.amount.isDefined) -1 else dialog_max
      val builder = titleBodyAsViewBuilder(title.asColoredView(R.color.cardBitcoinSigning), sendView.body)
      mkCheckFormNeutral(attempt, none, useMax, builder, dialog_ok, dialog_cancel, neutralRes)
    }

    lazy val feeView = new FeeView[GenerateTxResponse](FeeratePerByte(1L.sat), sendView.editView.fvc) {
      rate = WalletApp.feeRates.info.onChainFeeConf.feeEstimator.getFeeratePerKw(WalletApp.feeRates.info.onChainFeeConf.feeTargets.mutualCloseBlockTarget)

      worker = new ThrottledWork[String, GenerateTxResponse] {
        // This is a generic sending facility which may send to non-segwit, so always use a safer high dust threshold
        override def work(reason: String): GenerateTxResponse = ElectrumWallet.makeTx(specs, changeTo, pubKeyScript, sendView.rm.resultMsat.truncateToSatoshi, Map.empty, rate)
        override def error(exception: Throwable): Unit = update(feeOpt = None, showIssue = sendView.rm.resultMsat >= ElectrumWallet.params.dustLimit)
        override def process(reason: String, response: GenerateTxResponse): Unit = update(response.fee.toMilliSatoshi.asSome, showIssue = false)
      }

      override def update(feeOpt: Option[MilliSatoshi], showIssue: Boolean): Unit = UITask {
        updatePosButton(alert, isEnabled = feeOpt.isDefined).run
        super.update(feeOpt, showIssue)
      }.run
    }

    // Automatically update a candidate transaction each time user changes amount value
    sendView.rm.rmc.inputAmount addTextChangedListener onTextChange(feeView.worker.addWork)
    feeView.update(feeOpt = None, showIssue = false)

    uri.amount foreach { asked =>
      sendView.rm.updateText(value = asked)
      sendView.rm.rmc.inputAmount.setEnabled(false)
      sendView.rm.rmc.fiatInputAmount.setEnabled(false)
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
          val broadcastFuture = broadcastBtc(desc, signedTx, received = Satoshi(0L), sent = response.transferred, response.fee, incoming = 0)
          runFutureProcessOnUI(broadcastFuture, onFail) { case Some(error) => cleanFailedBroadcast(signedTx.txid.toHex, error.message) case None => }
          alert.dismiss
        }
      }

    lazy val alert = {
      val view = new TitleView(me getString dialog_send_btc_many).asColoredView(R.color.cardBitcoinSigning)
      mkCheckForm(attempt, none, titleBodyAsViewBuilder(view, sendView.body), dialog_ok, dialog_cancel)
    }

    lazy val feeView = new FeeView[GenerateTxResponse](FeeratePerByte(1L.sat), sendView.editView.fvc) {
      rate = WalletApp.feeRates.info.onChainFeeConf.feeEstimator.getFeeratePerKw(WalletApp.feeRates.info.onChainFeeConf.feeTargets.mutualCloseBlockTarget)

      worker = new ThrottledWork[String, GenerateTxResponse] {
        override def work(reason: String): GenerateTxResponse = ElectrumWallet.makeBatchTx(specs, changeTo, scriptToAmount, rate)
        override def process(reason: String, response: GenerateTxResponse): Unit = update(response.fee.toMilliSatoshi.asSome, showIssue = false)
        override def error(exception: Throwable): Unit = update(feeOpt = None, showIssue = true)
      }

      override def update(feeOpt: Option[MilliSatoshi], showIssue: Boolean): Unit = UITask {
        updatePosButton(alert, isEnabled = feeOpt.isDefined).run
        super.update(feeOpt, showIssue)
      }.run
    }

    for (address \ amount <- addressToAmount.values.reverse) {
      val humanAmount = BtcDenom.parsedTT(amount.toMilliSatoshi, cardIn, cardZero)
      val parent = getLayoutInflater.inflate(R.layout.frag_two_sided_item_gen, null)
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

  def bringTaSignInDialogEmail: Unit = {
    val title = new TitleView(me getString ta_login_title_email).asDefView
    val Tuple3(builder, extraInputLayout, extraInputField) = singleInputPopupBuilder(title)
    lazy val alert = mkCheckFormNeutral(proceed, none, signUpWarn, builder, dialog_ok, dialog_cancel, dialog_signup)

    lazy val listener = new LinkClient.Listener("login-email") {
      override def onDisconnected: Unit = updatePosButton(alert, isEnabled = true).run
      override def onResponse(args: Option[LinkClient.ResponseArguments] = None): Unit =
        if (args.isDefined) onDisconnected else moveToPass.run
    }

    def moveToPass = UITask {
      val email = extraInputField.getText.toString
      timer.schedule(bringTaSignInDialogPass(email), 225)
      alert.dismiss
    }

    def signUpWarn(unused: AlertDialog): Unit = {
      val title = new TitleView(me getString ta_signup_warn)
      val bld = titleBodyAsViewBuilder(null, title.asDefView)
      val bld1 = bld.setPositiveButton(dialog_ok, null)
      showForm(bld1.create)
    }

    def proceed(unused: AlertDialog): Unit = {
      updatePosButton(alert, isEnabled = false).run
      val loginReq = LinkClient.Login(None, extraInputField.getText.toString)
      WalletApp.linkClient ! LinkClient.Request(loginReq, listener.id)
      WalletApp.linkClient ! listener
    }

    extraInputLayout.setHint(ta_login_email)
    updatePosButton(alert, isEnabled = false).run
    extraInputField addTextChangedListener onTextChange { inputText =>
      val isEnabled = Patterns.EMAIL_ADDRESS.matcher(inputText).matches
      updatePosButton(alert, isEnabled).run
    }

    alert setOnDismissListener onDismiss {
      WalletApp.linkClient ! LinkClient.CmdRemove(listener)
    }
  }

  def bringTaSignInDialogPass(email: String) = UITask {
    val title = new TitleView(me getString ta_login_title_pass).asDefView
    val (builder, extraInputLayout, extraInputField) = singleInputPopupBuilder(title)
    lazy val alert = mkCheckForm(doLogin, none, builder, dialog_ok, dialog_cancel)

    lazy val listener = new LinkClient.Listener(LinkClient.USER_UPDATE) {
      override def onDisconnected: Unit = updatePosButton(alert, isEnabled = true).run
      override def onResponse(args: Option[LinkClient.ResponseArguments] = None): Unit = args.foreach {
        case _: LinkClient.Failure => onDisconnected // Something like a wrong password, allow to retry
        case _ => UITask(alert.dismiss).run // State update has already been handled, nothing to do
      }
    }

    def doLogin(unused: AlertDialog): Unit = {
      updatePosButton(alert, isEnabled = false).run
      val loginReq = LinkClient.Login(Some(extraInputField.getText.toString), email)
      WalletApp.linkClient ! LinkClient.Request(loginReq, listener.id)
      WalletApp.linkClient ! listener
    }

    extraInputLayout.setHint(ta_login_pass)
    updatePosButton(alert, isEnabled = false).run
    extraInputField addTextChangedListener onTextChange { inputText =>
      updatePosButton(alert, isEnabled = inputText.nonEmpty).run
    }

    alert setOnDismissListener onDismiss {
      WalletApp.linkClient ! LinkClient.CmdRemove(listener)
    }
  }

  def paymentAdapterDataChanged: TimerTask = UITask {
    // Do not show an exand button IF user has chosen to show full history OR searching is on OR we have nothing extra to show
    val expandHideCases = displayFullIxInfoHistory || isSearchOn || btcInfosToConsider.size + usdtInfosToConsider.size <= allInfos.size
    setVisMany(allInfos.nonEmpty -> walletCards.recentActivity, !expandHideCases -> expandContainer)
    paymentsAdapter.notifyDataSetChanged
  }

  def proceedConfirm(sendView: BtcSendView, alert: AlertDialog, response: GenerateTxResponse)(process: Transaction => Unit): Unit = {
    sendView.confirmView.confirmAmount.secondItem setText BtcDenom.parsedTT(response.transferred.toMilliSatoshi, cardIn, cardZero).html
    sendView.confirmView.confirmFiat.secondItem setText WalletApp.currentMsatInFiatHuman(response.transferred.toMilliSatoshi).html
    sendView.confirmView.confirmFee.secondItem setText BtcDenom.parsedTT(response.fee.toMilliSatoshi, cardIn, cardZero).html
    sendView.confirmView.chainEditButton setOnClickListener onButtonTap(sendView switchToDefault alert)
    sendView.confirmView.chainNextButton setOnClickListener onButtonTap(process apply response.tx)
    sendView.confirmView.chainCancelButton setOnClickListener onButtonTap(alert.dismiss)
    sendView.switchButtons(alert, on = false)
    sendView.switchTo(sendView.confirmView)
  }

  def proceedWithoutConfirm(alert: AlertDialog, response: GenerateTxResponse)(process: Transaction => Unit): Unit = {
    process(response.tx)
    alert.dismiss
  }

  def broadcastBtc(desc: BtcDescription, finalTx: Transaction, received: Satoshi, sent: Satoshi, fee: Satoshi, incoming: Int): Future[OkOrError] = {
    val info = BtcInfo(finalTx.toString, finalTx.txid.toHex, invalidPubKey.toString, depth = 0L, received, sent, fee, seenAt = System.currentTimeMillis,
      updatedAt = System.currentTimeMillis, desc, 0L.msat, WalletApp.fiatRates.info.rates.toJson.compactPrint, incoming, doubleSpent = 0L)
    WalletApp.pendingInfos(info.identity) = info
    WalletApp.seenInfos(info.identity) = info
    DbStreams.next(DbStreams.txStream)
    ElectrumWallet.broadcast(finalTx)
  }

  def broadcastUsdt(desc: UsdtDescription, req: Biconomy.TxDetailsRequest, amount: BigDecimal, fee: BigDecimal) = Future {
    val info = UsdtInfo(identity = CompleteUsdtWalletInfo.NOIDENTITY, network = UsdtDescription.POLYGON, block = Long.MaxValue, receivedUsdtString = "0",
      sentUsdtString = amount.toString, feeUsdtString = fee.toString, seenAt = System.currentTimeMillis, updatedAt = System.currentTimeMillis,
      desc, WalletApp.linkUsdt.data.totalBalance.toLong, incoming = 0L, doubleSpent = 0L)
    WalletApp.pendingInfos(desc.fromAddr) = info
    WalletApp.seenInfos(desc.fromAddr) = info
    DbStreams.next(DbStreams.txStream)
    WalletApp.biconomy.send(req)
  }

  def cleanFailedBroadcast(failedKey: String, message: String): Unit = {
    WalletApp.pendingInfos.remove(failedKey)
    WalletApp.seenInfos.remove(failedKey)
    DbStreams.next(DbStreams.txStream)
    onFail(message)
  }

  def walletAddresses = for {
    keys <- ElectrumWallet.specs.values.map(_.data.keys)
    (_, extPubKey) <- keys.accountKeyMap ++ keys.changeKeyMap
    privKey = keys.ewt.extPrivKeyFromPub(extPubKey).privateKey.value
  } yield BtcAddressAndPrivKey(keys.ewt.textAddress(extPubKey), privKey)

  def drongoNetwork = ElectrumWallet.chainHash match {
    case Block.LivenetGenesisBlock.hash => drongo.Network.MAINNET
    case Block.TestnetGenesisBlock.hash => drongo.Network.TESTNET
    case _ => drongo.Network.REGTEST
  }
}