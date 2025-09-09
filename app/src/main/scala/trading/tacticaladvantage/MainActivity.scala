package trading.tacticaladvantage

import android.content.Intent
import android.content.pm.PackageManager
import android.os.Bundle
import android.util.Patterns
import android.view.{View, ViewGroup}
import android.widget._
import androidx.appcompat.app.AlertDialog
import androidx.cardview.widget.CardView
import androidx.recyclerview.widget.RecyclerView
import com.ornach.nobobutton.NoboButton
import com.sparrowwallet.drongo
import fr.acinq.bitcoin.DeterministicWallet.ExtendedPublicKey
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
  lazy val contentWindow = findViewById(R.id.contentWindow).asInstanceOf[RelativeLayout]
  lazy val itemsList = findViewById(R.id.itemsList).asInstanceOf[ListView]

  lazy val expandContainer = getLayoutInflater.inflate(R.layout.frag_expand, null, false)
  lazy val expand = expandContainer.findViewById(R.id.expand).asInstanceOf[ImageButton]
  lazy val daysLeftRes = getResources getStringArray R.array.ta_days_left
  lazy val activeLoansRes = getResources getStringArray R.array.ta_loans

  lazy val paymentTypeIconIds =
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

  // BTC/USDT PAYMENT LINE

  // What happens after tx is generated and before user can end it?
  // Nothing by default, for TA deposits we need to inform a server first

  type ResponseToNothing = GenerateTxResponse => Unit
  type TxToSendProxy = (ResponseToNothing, SendView, AlertDialog) => ResponseToNothing
  val txToSendProxyNoop: TxToSendProxy = (fun, _, _) => response => fun(response)

  val txSendProxyTa: TxToSendProxy = (fun, sendView, alert) => response => {
    val listener = new LinkClient.Listener(id = "btc-deposit-intent") { self =>
      override def onResponse(args: Option[LinkClient.ResponseArguments] = None): Unit = {
        // We got a silent confirmation, no errors, can proceed further
        if (args.isEmpty) UITask(fun apply response).run
        onDisconnected
      }

      override def onDisconnected: Unit = {
        sendView.setInputEnabled(alert, isEnabled = true).run
        WalletApp.linkClient ! LinkClient.CmdRemove(self)
      }
    }

    alert setOnDismissListener onDismiss {
      val cmd = LinkClient.CmdRemove(listener)
      WalletApp.linkClient ! cmd
    }


    val intent = LinkClient.DepositIntent(response.tx.txid.toHex, LinkClient.BTC)
    WalletApp.linkClient ! LinkClient.Request(intent, listener.id)
    WalletApp.linkClient ! listener
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

      val sendView = new BtcSendView(specs, MAX_MSAT)
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
          val desc = PlainBtcDescription(address :: Nil, label = None, cpfpBumpOrder.asSome, cpfpBy = None, cpfpOf = info.txid.asSome)
          alert.dismiss

          runFutureProcessOnUI(broadcastBtc(desc, response.tx, response.transferred, sent = Satoshi(0L), response.fee, incoming = 1), onFail) {
            case None => WalletApp.btcTxDataBag.updDescription(parentDescWithOrder.withNewCPFPBy(response.tx.txid), info.txid)
            case Some(error) => cleanFailedBtcBroadcast(info, response.tx.txid.toHex, error.message)
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
      case specs if specs.size < info.extPubs.size => WalletApp.app.quickToast(error_no_wallet)
      case specs if info.description.taRoi.isDefined => doBoostRBF(specs, info, txSendProxyTa)
      case specs => doBoostRBF(specs, info, txToSendProxyNoop)
    }

    def doBoostRBF(specs: Seq[WalletSpec], info: BtcInfo, txToSendProxy: TxToSendProxy): Unit = {
      val currentFee = BtcDenom.parsedTT(info.feeSat.toMilliSatoshi, cardIn, cardZero)
      val changeTo = ElectrumWallet.orderByImportance(candidates = specs).head
      val sendView = new BtcSendView(specs, MAX_MSAT)

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

      def attempt(alert1: AlertDialog): Unit = {
        // Transaction could have gotten a confirmation while user was filling a form
        val sanityCheck = ElectrumWallet.doubleSpent(specs.head.data.keys.ewt.xPub, info.tx)
        if (sanityCheck.depth > 0 || sanityCheck.isDoubleSpent) return

        val rbfParams = RBFParams(info.txid, BtcDescription.RBF_BOOST)
        val ofOriginalTxid = info.description.rbf.map(_.ofTxid).getOrElse(info.txid)
        val rbfBumpOrder = SemanticOrder(ofOriginalTxid.toHex, -System.currentTimeMillis)

        def proceedBroadcastWithoutConfirm(response: GenerateTxResponse): Unit = runAnd(alert1.dismiss) {
          val desc = PlainBtcDescription(Nil, None, rbfBumpOrder.asSome, None, None, rbfParams.asSome, taRoi = info.description.taRoi)
          runFutureProcessOnUI(broadcastBtc(desc, response.tx, received = Satoshi(0L), info.sentSat, response.fee, incoming = 0), onFail) {
            case None => WalletApp.btcTxDataBag.updDescription(info.description.withNewOrderCond(rbfBumpOrder.copy(order = Long.MaxValue).asSome), info.txid)
            case Some(error) => cleanFailedBtcBroadcast(info, response.tx.txid.toHex, error.message)
          }
        }

        sendView.setInputEnabled(alert1, isEnabled = false).run
        val proceed: ResponseToNothing = txToSendProxy(proceedBroadcastWithoutConfirm, sendView, alert1)
        runInFutureProcessOnUI(ElectrumWallet.rbfBump(specs, changeTo, info.tx, feeView.rate).result.right.get, onFail)(proceed)
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

      val sendView = new BtcSendView(specs, MAX_MSAT)
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

        runInFutureProcessOnUI(ElectrumWallet.rbfReroute(specs, info.tx, feeView.rate, ourPubKeyScript).result.right.get, onFail) { response =>
          val desc = PlainBtcDescription(addresses = Nil, label = None, rbfBumpOrder.asSome, cpfpBy = None, cpfpOf = None, rbf = rbfParams.asSome)
          alert.dismiss

          runFutureProcessOnUI(broadcastBtc(desc, response.tx, info.sentSat - response.fee, sent = 0L.sat, response.fee, incoming = 1), onFail) {
            case None => WalletApp.btcTxDataBag.updDescription(info.description.withNewOrderCond(rbfBumpOrder.copy(order = Long.MaxValue).asSome), info.txid)
            case Some(error) => cleanFailedBtcBroadcast(info, response.tx.txid.toHex, error.message)
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
          for (usdtLabel <- info.description.label) addFlowChip(extraInfo, usdtLabel, R.drawable.border_yellow, None)
          addFlowChip(extraInfo, getString(popup_txid).format(info.identity.short0x), R.drawable.border_gray) {
            me browse s"https://polygonscan.com/tx/${info.identity}"
          }

        case info: BtcInfo =>
          val canRBF = !info.isIncoming && !info.isDoubleSpent && !info.isConfirmed && info.description.cpfpOf.isEmpty
          val canCPFP = info.isIncoming && !info.isDoubleSpent && !info.isConfirmed && info.description.rbf.isEmpty && info.description.canBeCPFPd

          for (btcLabel <- info.description.label) addFlowChip(extraInfo, btcLabel, R.drawable.border_white, None)
          addFlowChip(extraInfo, getString(popup_txid).format(info.identity.short), R.drawable.border_gray) {
            me browse s"https://mempool.space/tx/${info.identity}"
          }

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
      lazy val taClientCard = new TaWalletCard {
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
        for (xPub <- ElectrumWallet.specs.keys) yield new BtcWalletCard(xPub) {
          override def onTap: Unit = goToWithValue(ClassNames.qrBtcActivityClass, xPub)
          override def hide: Unit = WalletApp.removeBtcWallet(key = xPub)
        }

      val usdtCards =
        for (info <- WalletApp.linkUsdt.data.wallets) yield new UsdtWalletCard(info.xPriv) {
          override def hide: Unit = WalletApp.linkUsdt ! LinkUsdt.CmdRemoveWallet(xPriv = info.xPriv)
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
    if (reqCode == scannerRequestCode && results.nonEmpty && results.head == PackageManager.PERMISSION_GRANTED)
      bringScanner(null)

  override def checkExternalData(whenNone: Runnable): Unit = InputParser.checkAndMaybeErase {
    case pbu: PlainBitcoinUri if Try(ElectrumWallet addressToPubKeyScript pbu.address).isSuccess =>
      bringSingleAddressSelector(pbu, plainTitle, txToSendProxyNoop).run

    case a2a: MultiAddressParser.AddressToAmount if a2a.values.nonEmpty =>
      val dustAmount = a2a.values.secondItems.find(amount => ElectrumWallet.params.dustLimit > amount)
      val badAddress = a2a.values.firstItems.find(address => Try(ElectrumWallet addressToPubKeyScript address).isFailure)

      if (badAddress.nonEmpty) onFail(s"Incorrect address=${badAddress.get}")
      else if (dustAmount.nonEmpty) onFail(s"Low amount=${dustAmount.get.toLong}")
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
      sendView.setButtonsVisible(alert, on = false)
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

  def plainTitle(uri: PlainBitcoinUri): TitleView = {
    val label = uri.label.map(label => s"<br><br><b>$label</b>").getOrElse(new String)
    val caption = getString(dialog_send_btc).format(uri.address.short, label)
    new TitleView(caption)
  }

  def loanTitle(loan: LinkClient.LoanAd): TitleView = {
    val titleView = new TitleView(s"<b>${me getString ta_loan}</b>")
    val durationHuman = WalletApp.app.plurOrZero(daysLeftRes, loan.durationDays.toInt)
    val parentDuration = getLayoutInflater.inflate(R.layout.frag_two_sided_item_ta, null)
    val parentAPR = getLayoutInflater.inflate(R.layout.frag_two_sided_item_ta, null)

    titleView.view.addView(new TwoSidedItem(parentDuration, getString(ta_loan_duration), durationHuman).parent)
    titleView.view.addView(new TwoSidedItem(parentAPR, getString(ta_loan_annual_return), Denomination.formatRoi format loan.roi).parent)
    addFlowChip(titleView.flow, "tactical-advantage.trading", R.drawable.border_white)(me browse "tactical-advantage.trading")
    titleView
  }

  def proceedConfirm(sendView: BtcSendView, desc: BtcDescription, alert: AlertDialog, response: GenerateTxResponse): Unit = {
    sendView.confirmView.confirmAmount.secondItem setText BtcDenom.parsedTT(response.transferred.toMilliSatoshi, cardIn, cardZero).html
    sendView.confirmView.confirmFiat.secondItem setText WalletApp.currentMsatInFiatHuman(response.transferred.toMilliSatoshi).html
    sendView.confirmView.confirmFee.secondItem setText BtcDenom.parsedTT(response.fee.toMilliSatoshi, cardIn, cardZero).html
    sendView.confirmView.chainEditButton setOnClickListener onButtonTap(sendView switchToDefault alert)
    sendView.confirmView.chainCancelButton setOnClickListener onButtonTap(alert.dismiss)
    // Transition to final confirmation dialog within the same alert
    sendView.setButtonsVisible(alert, on = false)
    sendView.switchTo(sendView.confirmView)

    sendView.confirmView.chainNextButton setOnClickListener onButtonTap {
      val broadcastFuture = broadcastBtc(desc, response.tx, received = Satoshi(0L), sent = response.transferred, response.fee, incoming = 0)
      runFutureProcessOnUI(broadcastFuture, onFail) { case Some(error) => cleanFailedBroadcast(response.tx.txid.toHex, error.message) case None => }
      alert.dismiss
    }
  }

  def bringSingleAddressSelector[T <: BitcoinUri](pbu: T, makeTitle: T => TitleView, txToSendProxy: TxToSendProxy) = UITask {
    val pubKeyScript = ElectrumWallet.addressToPubKeyScript(pbu.address)

    new BtcWalletSelector(makeTitle apply pbu) {
      def onOk(specs: List[WalletSpec] = Nil): Unit = {
        val sendView = new BtcSendView(specs, pbu.maxAmount)
        val changeTo = ElectrumWallet.orderByImportance(specs).head

        def attempt(alert1: AlertDialog): Unit = {
          sendView.setInputEnabled(alert1, isEnabled = false).run
          val sendAmount = sendView.rm.resultMsat.truncateToSatoshi
          val proceed = txToSendProxy(proceedConfirm(sendView, pbu.desc, alert1, _: GenerateTxResponse), sendView, alert1)
          runInFutureProcessOnUI(ElectrumWallet.makeTx(specs, changeTo, pubKeyScript, sendAmount, Map.empty, feeView.rate), onFail)(proceed)
        }

        lazy val alert = {
          val builder = titleBodyAsViewBuilder(makeTitle(pbu).asColoredView(R.color.cardBitcoinSigning), sendView.body)
          mkCheckFormNeutral(attempt, none, _ => sendView.rm.updateText(sendView.totalCanSend), builder, dialog_ok, dialog_cancel,
            neutralRes = if (pbu.amount.isDefined) -1 else dialog_max)
        }

        lazy val feeView = new FeeView[GenerateTxResponse](FeeratePerByte(1L.sat), sendView.editView.fvc) {
          val mutualCloseBlockTarget = WalletApp.feeRates.info.onChainFeeConf.feeTargets.mutualCloseBlockTarget
          rate = WalletApp.feeRates.info.onChainFeeConf.feeEstimator.getFeeratePerKw(mutualCloseBlockTarget)

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

        pbu.amount.foreach { asked =>
          sendView.rm.updateText(value = asked)
          sendView.rm.rmc.inputAmount.setEnabled(false)
          sendView.rm.rmc.fiatInputAmount.setEnabled(false)
        }
      }
    }
  }

  def bringMultiAddressSelector(a2a: MultiAddressParser.AddressToAmount) = new BtcWalletSelector(me getString dialog_send_btc_many) {
    val scriptToAmount = a2a.values.firstItems.map(ElectrumWallet.addressToPubKeyScript).zip(a2a.values.secondItems).toMap

    def onOk(specs: List[WalletSpec] = Nil): Unit = {
      val changeTo = ElectrumWallet.orderByImportance(specs).head
      val sendView = new BtcSendView(specs, hardMax = MAX_MSAT)

      def attempt(alert1: AlertDialog): Unit = {
        val proceed = proceedConfirm(sendView, PlainBtcDescription(a2a.values.firstItems.toList), alert1, _: GenerateTxResponse)
        runInFutureProcessOnUI(ElectrumWallet.makeBatchTx(specs, changeTo, scriptToAmount, feeView.rate), onFail)(proceed)
      }

      lazy val alert = {
        val view = new TitleView(me getString dialog_send_btc_many).asColoredView(R.color.cardBitcoinSigning)
        mkCheckForm(attempt, none, titleBodyAsViewBuilder(view, sendView.body), dialog_ok, dialog_cancel)
      }

      lazy val feeView = new FeeView[GenerateTxResponse](FeeratePerByte(1L.sat), sendView.editView.fvc) {
        val mutualCloseBlockTarget = WalletApp.feeRates.info.onChainFeeConf.feeTargets.mutualCloseBlockTarget
        rate = WalletApp.feeRates.info.onChainFeeConf.feeEstimator.getFeeratePerKw(mutualCloseBlockTarget)

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

      for (address \ amount <- a2a.values.reverse) {
        val humanAmount = BtcDenom.parsedTT(amount.toMilliSatoshi, cardIn, cardZero)
        val parent = getLayoutInflater.inflate(R.layout.frag_two_sided_item_gen, null)
        new TwoSidedItem(parent, address.short.html, humanAmount.html)
        sendView.editChain.addView(parent, 0)
      }

      setVis(isVisible = false, sendView.inputChain)
      feeView.update(feeOpt = None, showIssue = false)
      feeView.worker addWork "MULTI-SEND-INIT-CALL"
    }
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
      override def onDisconnected: Unit = onText(extraInputField.getText.toString)
      override def onResponse(args: Option[LinkClient.ResponseArguments] = None): Unit =
        if (args.isEmpty) moveToPass.run else onDisconnected
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
    }

    def onText(inputText: String) = {
      val matcher = Patterns.EMAIL_ADDRESS.matcher(inputText)
      updatePosButton(alert, matcher.matches).run
    }

    WalletApp.linkClient ! listener
    extraInputLayout.setHint(ta_login_email)
    updatePosButton(alert, isEnabled = false).run
    extraInputField addTextChangedListener onTextChange(onText)

    alert setOnDismissListener onDismiss {
      val cmd = LinkClient.CmdRemove(listener)
      WalletApp.linkClient ! cmd
    }
  }

  def bringTaSignInDialogPass(email: String) = UITask {
    val title = new TitleView(me getString ta_login_title_pass).asDefView
    val (builder, extraInputLayout, extraInputField) = singleInputPopupBuilder(title)
    lazy val alert = mkCheckForm(doLogin, none, builder, dialog_ok, dialog_cancel)

    lazy val listener = new LinkClient.Listener(LinkClient.USER_UPDATE) {
      override def onDisconnected: Unit = onText(extraInputField.getText.toString)
      override def onResponse(args: Option[LinkClient.ResponseArguments] = None): Unit = args.foreach {
        case _: LinkClient.Failure => onDisconnected // Something like a wrong password, allow to retry
        case _ => UITask(alert.dismiss).run // State update has already been handled, nothing to do
      }
    }

    def doLogin(unused: AlertDialog): Unit = {
      updatePosButton(alert, isEnabled = false).run
      val loginReq = LinkClient.Login(Some(extraInputField.getText.toString), email)
      WalletApp.linkClient ! LinkClient.Request(loginReq, listener.id)
    }

    def onText(inputText: String) = {
      updatePosButton(alert, inputText.nonEmpty).run
    }

    WalletApp.linkClient ! listener
    extraInputLayout.setHint(ta_login_pass)
    updatePosButton(alert, isEnabled = false).run
    extraInputField addTextChangedListener onTextChange(onText)

    alert setOnDismissListener onDismiss {
      val cmd = LinkClient.CmdRemove(listener)
      WalletApp.linkClient ! cmd
    }
  }

  def paymentAdapterDataChanged: TimerTask = UITask {
    // Do not show an exand button IF user has chosen to show full history OR searching is on OR we have nothing extra to show
    val expandHideCases = displayFullIxInfoHistory || isSearchOn || btcInfosToConsider.size + usdtInfosToConsider.size <= allInfos.size
    setVisMany(allInfos.nonEmpty -> walletCards.recentActivity, !expandHideCases -> expandContainer)
    paymentsAdapter.notifyDataSetChanged
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

  def cleanFailedBtcBroadcast(info: BtcInfo, failedKey: String, message: String): Unit = {
    WalletApp.btcTxDataBag.updDescription(info.description, info.txid)
    cleanFailedBroadcast(failedKey, message)
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

  // WALLET CARDS

  abstract class BtcWalletSelector(title: TitleView) { self =>
    val spendable = ElectrumWallet.specs.values.filter(_.spendable).toList
    val usable = ElectrumWallet.specs.values.filter(_.usable).toList
    def onOk(specs: List[WalletSpec] = Nil): Unit

    if (spendable.size == 1) onOk(spendable)
    else if (usable.size == 1) onOk(usable)
    else {
      val info = addFlowChip(title.flow, getString(select_wallets), R.drawable.border_yellow, None)
      val cardsContainer = getLayoutInflater.inflate(R.layout.frag_linear_layout, null).asInstanceOf[LinearLayout]
      lazy val alert = mkCheckForm(proceed, none, titleBodyAsViewBuilder(title.view, cardsContainer), dialog_ok, dialog_cancel)

      def proceed(alert1: AlertDialog): Unit = runAnd(alert1.dismiss)(self onOk chosenSpecs.toList)
      def chosenSpecs: Seq[WalletSpec] = cards.filter(_.isSelected).map(_.xPub).flatMap(ElectrumWallet.specs.get)

      lazy val cards = for (spec <- spendable) yield
        new BtcWalletCard(xPub = spec.data.keys.ewt.xPub) {
          setVisMany(false -> cardButtons, false -> infoWalletNotice)

          def onTap: Unit = {
            isSelected = !isSelected
            val totalCanSend = chosenSpecs.map(_.info.lastBalance).sum.toMilliSatoshi
            val formatted = "<small>∑ </small>" + BtcDenom.parsedTT(totalCanSend, cardIn, cardZero)
            if (totalCanSend > 0L.msat) info.setText(formatted.html) else info.setText(select_wallets)
            updatePosButton(alert, isEnabled = chosenSpecs.nonEmpty).run
            updateView
          }
        }

      updatePosButton(alert, isEnabled = false).run
      val chooser = new WalletCardManager(cardsContainer)
      chooser.init(cards).cardViews.foreach(_.updateView)
      chooser.unPad
    }
  }

  class WalletCardManager(holder: LinearLayout) {
    var cardViews = List.empty[WalletCard]

    def init(cards: List[WalletCard] = Nil): WalletCardManager = {
      cards.foreach(holder addView _.cardWrap)
      cardViews = cards
      this
    }

    def unPad: Unit = cardViews.foreach { card =>
      val padding: Int = card.cardWrap.getPaddingTop
      card.cardWrap.setPadding(padding, padding, padding, 0)
    }
  }

  abstract class WalletCard {
    val cardWrap: LinearLayout = getLayoutInflater.inflate(R.layout.frag_wallet_card, null).asInstanceOf[LinearLayout]
    val imageTip: ImageView = cardWrap.findViewById(R.id.imageTip).asInstanceOf[ImageView]
    val cardView: CardView = cardWrap.findViewById(R.id.cardView).asInstanceOf[CardView]
    cardView setOnClickListener onButtonTap(onTap)

    val infoContainer: View = cardWrap.findViewById(R.id.infoContainer).asInstanceOf[View]
    val infoWalletLabel: TextView = cardWrap.findViewById(R.id.infoWalletLabel).asInstanceOf[TextView]
    val infoWalletNotice: TextView = cardWrap.findViewById(R.id.infoWalletNotice).asInstanceOf[TextView]
    infoWalletNotice setText tap_to_receive

    val balanceContainer: LinearLayout = cardWrap.findViewById(R.id.balanceContainer).asInstanceOf[LinearLayout]
    val balanceWalletFiat: TextView = cardWrap.findViewById(R.id.balanceWalletFiat).asInstanceOf[TextView]
    val balanceWallet: TextView = cardWrap.findViewById(R.id.balanceWallet).asInstanceOf[TextView]
    val cardButtons: FlowLayout = cardWrap.findViewById(R.id.cardButtons).asInstanceOf[FlowLayout]
    addFlowChip(cardButtons, getString(dialog_hide), R.drawable.border_blue)(hide)

    def hide: Unit = none
    def updateView: Unit
    def onTap: Unit
  }

  abstract class BtcWalletCard(val xPub: ExtendedPublicKey) extends WalletCard {
    imageTip setImageResource R.drawable.add_24
    var isSelected: Boolean = false

    def updateView: Unit = {
      val spec = ElectrumWallet.specs(xPub)
      val hasMoney = spec.info.lastBalance.toLong > 0L
      val bgResource = if (isSelected) R.drawable.border_white else R.color.cardBitcoinSigning
      val attachment = if (spec.info.core.attachedMaster.isDefined) R.drawable.attachment_24 else 0
      infoWalletLabel setText spec.info.label.asSome.filter(_.trim.nonEmpty).getOrElse(me getString bitcoin_wallet)
      balanceWallet setText BtcDenom.parsedTT(spec.info.lastBalance.toMilliSatoshi, "#FFFFFF", signCardZero).html
      balanceWalletFiat setText WalletApp.currentMsatInFiatHuman(spec.info.lastBalance.toMilliSatoshi)
      infoWalletLabel.setCompoundDrawablesWithIntrinsicBounds(0, 0, attachment, 0)
      setVisMany(hasMoney -> balanceContainer, !hasMoney -> imageTip)
      infoContainer setBackgroundResource bgResource
    }
  }

  abstract class UsdtWalletCard(val xPriv: String) extends WalletCard {
    infoContainer setBackgroundResource R.color.usdt
    imageTip setImageResource R.drawable.add_24

    def updateView: Unit = {
      val info = WalletApp.linkUsdt.data.wallets.find(_.xPriv == xPriv).get
      infoWalletLabel setText info.label.asSome.filter(_.trim.nonEmpty).getOrElse(me getString usdt_wallet)
      balanceWallet setText Denomination.fiatTT(info.lastBalance, "0", "#FFFFFF", signCardZero, isIncoming = true).html
      setVisMany(info.isDust -> imageTip, !info.isDust -> balanceContainer, false -> balanceWalletFiat)
    }
  }

  abstract class TaWalletCard extends WalletCard {
    infoWalletLabel setText ta_earn_label

    val earnAccount = new EarnAccount
    cardView.addView(earnAccount.wrap, 0)

    def updateView: Unit = {
      WalletApp.linkClient.data match {
        case status: LinkClient.UserStatus =>
          infoWalletNotice setText status.email
          imageTip.setImageResource(R.drawable.info_24)
          val minDaysLeft = (status.activeLoans.map(_.daysLeft) :+ 0L).minBy(identity)
          balanceWalletFiat setText WalletApp.app.plurOrZero(daysLeftRes, minDaysLeft.toInt)
          balanceWallet setText WalletApp.app.plurOrZero(activeLoansRes, status.activeLoans.size)
          setVis(isVisible = status.activeLoans.nonEmpty, balanceContainer)
          setVis(isVisible = status.activeLoans.isEmpty, imageTip)
          earnAccount.updateView(status)
        case LinkClient.LoggedOut if earnAccount.isExpanded =>
          androidx.transition.TransitionManager.beginDelayedTransition(cardWrap)
          setVisMany(false -> earnAccount.wrap, true -> infoContainer)
          updateView
        case LinkClient.LoggedOut =>
          infoWalletNotice setText ta_client_login
          imageTip.setImageResource(R.drawable.lock_24)
          setVis(isVisible = false, balanceContainer)
          setVis(isVisible = true, imageTip)
      }
    }
  }

  class EarnAccount {
    val wrap: LinearLayout = getLayoutInflater.inflate(R.layout.frag_ta_account, null).asInstanceOf[LinearLayout]
    val taBalancesContainer: LinearLayout = wrap.findViewById(R.id.taBalancesContainer).asInstanceOf[LinearLayout]
    val taLoansContainer: LinearLayout = wrap.findViewById(R.id.taLoansContainer).asInstanceOf[LinearLayout]
    val taClientEmail: TextView = wrap.findViewById(R.id.taClientEmail).asInstanceOf[TextView]
    val taExtended: FlowLayout = wrap.findViewById(R.id.taExtended).asInstanceOf[FlowLayout]
    val taBalancesTitle: View = wrap.findViewById(R.id.taBalancesTitle).asInstanceOf[View]
    val taDeposit: NoboButton = wrap.findViewById(R.id.taDeposit).asInstanceOf[NoboButton]
    val taLoansTitle: View = wrap.findViewById(R.id.taLoansTitle).asInstanceOf[View]
    def isExpanded: Boolean = wrap.getVisibility == View.VISIBLE

    val loanAdListener = new LinkClient.Listener("get-loan-ad") {
      override def onResponse(args: Option[LinkClient.ResponseArguments] = None): Unit = {
        val showForm = bringSingleAddressSelector(_: LinkClient.LoanAd, loanTitle, txSendProxyTa)
        args.collectFirst { case data: LinkClient.LoanAd => showForm(data).run }
        onDisconnected
      }

      override def onDisconnected: Unit = {
        WalletApp.linkClient ! LinkClient.CmdRemove(this)
        UITask(taDeposit setEnabled true).run
      }
    }

    taDeposit setOnClickListener onButtonTap {
      val getLoanAd = LinkClient.GetLoanAd(asset = LinkClient.BTC)
      WalletApp.linkClient ! LinkClient.Request(getLoanAd, loanAdListener.id)
      WalletApp.linkClient ! loanAdListener
      taDeposit.setEnabled(false)
    }

    def updateView(status: LinkClient.UserStatus): Unit = {
      val balances = status.totalFunds.filter(_.withdrawable > 0D)
      List(taBalancesContainer, taLoansContainer, taExtended).foreach(_.removeAllViewsInLayout)
      setVisMany(balances.nonEmpty -> taBalancesTitle, status.activeLoans.nonEmpty -> taLoansTitle)
      taClientEmail.setText(status.email)

      for (balance <- balances) {
        val parent = getLayoutInflater.inflate(R.layout.frag_two_sided_item_ta, null)
        val item = new TwoSidedItem(parent, getString(balance.currency), balance.amountHuman.html)
        item.firstItem.setCompoundDrawablesWithIntrinsicBounds(balance.icon, 0, 0, 0)
        taBalancesContainer.addView(parent)
      }

      for (loan <- status.activeLoans) {
        val daysLeft = WalletApp.app.plurOrZero(daysLeftRes, loan.daysLeft.toInt)
        val amounts = s"${loan.amountHuman}<br><small>${loan.interestHuman}</small>"
        val parent = getLayoutInflater.inflate(R.layout.frag_two_sided_item_ta, null)
        val details = s"APR ${Denomination.formatRoi format loan.roi}<br><small><tt>$daysLeft</tt></small>"
        val item = new TwoSidedItem(parent, firstText = details.html, secondText = amounts.html)
        item.firstItem.setCompoundDrawablesWithIntrinsicBounds(loan.icon, 0, 0, 0)
        taLoansContainer.addView(parent)
      }

      (balances.nonEmpty, status.pendingWithdraws.nonEmpty) match {
        case (true, true) => addFlowChip(taExtended, getString(ta_withdraw_on), R.drawable.border_yellow)(none)
        case (true, false) => addFlowChip(taExtended, getString(ta_withdraw_off), R.drawable.border_yellow)(none)
        case _ =>
      }

      addFlowChip(taExtended, getString(ta_support), R.drawable.border_blue)(me browse "mailto:contact@tactical-advantage.trading")
      addFlowChip(taExtended, getString(ta_logout), R.drawable.border_blue)(WalletApp.linkClient ! LinkClient.LoggedOut)
    }
  }
}