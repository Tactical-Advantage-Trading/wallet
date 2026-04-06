package trading.tacticaladvantage

import android.content.Intent
import android.content.pm.PackageManager
import android.net.Uri
import android.os.Bundle
import android.util.Patterns
import android.view.{View, ViewGroup}
import android.widget._
import androidx.appcompat.app.AlertDialog
import androidx.cardview.widget.CardView
import androidx.core.app.ActivityCompat
import androidx.recyclerview.widget.RecyclerView
import fr.acinq.bitcoin.DeterministicWallet.{ExtendedPrivateKey, ExtendedPublicKey}
import fr.acinq.bitcoin._
import fr.acinq.eclair._
import fr.acinq.eclair.blockchain.electrum.ElectrumWallet._
import fr.acinq.eclair.blockchain.electrum.{ElectrumWallet, WalletSpec}
import fr.acinq.eclair.blockchain.fee.FeeratePerByte
import immortan.Tools._
import immortan._
import immortan.sqlite.{DbStreams, SQLiteTx}
import immortan.utils.ImplicitJsonFormats._
import immortan.utils._
import org.apmem.tools.layouts.FlowLayout
import rx.lang.scala.Subscription
import spray.json._
import trading.tacticaladvantage.BaseActivity.StringOps
import trading.tacticaladvantage.LinkClient.{FullBalance, PartialInterestNative}
import trading.tacticaladvantage.MainActivity._
import trading.tacticaladvantage.R.string._
import trading.tacticaladvantage.utils._

import java.util.TimerTask
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.{Failure, Success}

object MainActivity {
  case class Accumulator(identities: Set[String], infos: Set[ItemDetails] = Set.empty) {
    def withInfo(info: CoinDetails): Accumulator = Accumulator(identities ++ info.relatedTxids, infos + info)
  }

  val DEFAULT_SHOW_ITEMS = 1
  var displayFullIxInfoHistory: Boolean = false
  var idsToDisplayAnyway: Set[String] = Set.empty
  var btcInfosToConsider: Iterable[CoinDetails] = Nil
  var ecxInfosToConsider: Iterable[CoinDetails] = Nil
  var allInfos: Seq[ItemDetails] = Nil
}

class MainActivity extends BaseActivity with MnemonicActivity with ExternalDataChecker { me =>
  lazy val paymentTypeIconIds = List(R.id.btcInOut, R.id.ecxInOut, R.id.inBoosted, R.id.outBoosted, R.id.outCancelled)
  lazy val contentWindow = findViewById(R.id.contentWindow).asInstanceOf[RelativeLayout]
  lazy val itemsList = findViewById(R.id.itemsList).asInstanceOf[ListView]

  lazy val expandContainer = getLayoutInflater.inflate(R.layout.frag_expand, null, false)
  lazy val expand = expandContainer.findViewById(R.id.expand).asInstanceOf[ImageButton]
  lazy val daysLeftRes = getResources getStringArray R.array.ta_days_left
  lazy val activeLoansRes = getResources getStringArray R.array.ta_loans

  lazy val walletCards = new WalletCardsViewHolder
  var openListItems = Set.empty[String]

  // PAYMENT LIST

  def loadRecent: Unit = {
    btcInfosToConsider = WalletApp.btc.txDataBag.listRecentTxs(5).map(SQLiteTx.reify)
    ecxInfosToConsider = WalletApp.ecx.txDataBag.listRecentTxs(5).map(SQLiteTx.reify)
    var dbInfos: Iterable[ItemDetails] = btcInfosToConsider ++ ecxInfosToConsider

    if (!displayFullIxInfoHistory) {
      // First, select a number of most recent chronological items to be displayed by default
      val sortedCandidates = dbInfos.toList.sortBy(_.seenAt)(Ordering[Long].reverse).take(DEFAULT_SHOW_ITEMS)

      // Then, we init accumulator with those
      val accumulator1 = Accumulator(idsToDisplayAnyway)
      val accumulator2 = sortedCandidates.foldLeft(accumulator1) {
        case (acc, info: CoinDetails) => acc.withInfo(info)
      }

      // Then see if any dropped ones should be displayed
      val accumulator3 = dbInfos.foldLeft(accumulator2) {
        case (acc, info: CoinDetails) if acc.identities.intersect(info.relatedTxids).nonEmpty => acc.withInfo(info)
        case (acc, info: CoinDetails) if idsToDisplayAnyway.contains(info.identity) => acc.withInfo(info)
        case (acc, info: CoinDetails) if !info.isConfirmed && !info.isDoubleSpent => acc.withInfo(info)
        case (acc, _) => acc
      }

      // Allows reduced list to grow (e.g. 3; got new tx; 4)
      idsToDisplayAnyway ++= accumulator3.identities
      dbInfos = accumulator3.infos
    }

    val displayed = WalletApp.pendingInfos.values ++ dbInfos
    allInfos = SemanticOrder.makeSemanticOrder(displayed.toList)
  }

  val paymentsAdapter: BaseAdapter = new BaseAdapter {
    override def getItem(pos: Int): ItemDetails = allInfos(pos)
    override def getItemId(position: Int): Long = position
    override def getCount: Int = allInfos.size

    override def getView(pos: Int, savedView: View, parent: ViewGroup): View = getItem(pos) match { case item =>
      val view = if (null == savedView) getLayoutInflater.inflate(R.layout.frag_payment_line, null) else savedView
      val holder = if (null == view.getTag) new PaymentLineViewHolder(view) else view.getTag.asInstanceOf[PaymentLineViewHolder]
      setVis(item.isExpandedItem, holder.spacer)
      holder.updateDetails(item)
      view
    }
  }

  // BTC/USDT PAYMENT LINE

  // What happens after tx is generated and before user can send it?
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


    val intent = LinkClient.DepositIntent(response.tx.txid.toHex)
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
    var currentGroup: NetworkWalletGroup = _
    var currentDetails: ItemDetails = _
    var lastVisibleIconId: Int = -1

    paymentCardContainer setOnClickListener onButtonTap {
      if (extraInfo.getVisibility == View.VISIBLE) collapse else expand
    }

    // CPFP / RBF

    def boostCPFP(info: CoinDetails): Unit = info.extPubs.flatMap(currentGroup.electrum.specs.get) match {
      case Nil => WalletApp.app.quickToast(error_no_wallet) case wallets => doBoostCPFP(wallets, info)
    }

    def doBoostCPFP(specs: Seq[WalletSpec], info: CoinDetails): Unit = {
      val changeSpec = ElectrumWallet.orderByImportance(candidates = specs).head
      val address = changeSpec.data.keys.ewt.textAddress(changeSpec.data.changePubKey)
      val ourPubKeyScript = currentGroup.electrum.addressToPubKeyScript(address)

      val fromOutPoints = for (idx <- info.tx.txOut.indices) yield OutPoint(info.tx.hash, idx)
      val receivedMsat = info.receivedSat.toMilliSatoshi

      val sendView = new BtcSendView(currentGroup.fiatRates, specs, MAX_MSAT)
      val blockTarget = currentGroup.feeRates.info.onChainFeeConf.feeTargets.fundingBlockTarget
      val target = currentGroup.feeRates.info.onChainFeeConf.feeEstimator.getFeeratePerKw(blockTarget)
      lazy val feeView = new FeeView[GenerateTxResponse](currentGroup.fiatRates, FeeratePerByte(target), sendView.cpfpView.fvc) {
        rate = target

        worker = new ThrottledWork[String, GenerateTxResponse] {
          override def work(reason: String): GenerateTxResponse = currentGroup.electrum.makeCPFP(specs, fromOutPoints.toSet, ourPubKeyScript, rate)
          override def process(reason: String, response: GenerateTxResponse): Unit = update(response.fee.toMilliSatoshi.asSome, showIssue = false)
          override def error(exc: Throwable): Unit = update(feeOpt = None, showIssue = true)
        }

        override def update(feeOpt: Option[MilliSatoshi], showIssue: Boolean): Unit = UITask {
          val currentAmount = CoinDenom.directedTT(incoming = receivedMsat, outgoing = 0L.msat, cardOut, cardIn, cardZero, isIncoming = true)
          val afterAmount = CoinDenom.directedTT(feeOpt.map(receivedMsat.-).getOrElse(receivedMsat), 0L.msat, cardOut, cardIn, cardZero, isIncoming = true)
          sendView.cpfpView.cpfpCurrent.secondItem.setText(currentAmount.html)
          sendView.cpfpView.cpfpAfter.secondItem.setText(afterAmount.html)
          updatePosButton(alert, feeOpt.isDefined).run
          super.update(feeOpt, showIssue)
        }.run
      }

      def attempt(alert: AlertDialog): Unit = {
        // Transaction could have gotten a confirmation while user was filling a form
        val sanityCheck = currentGroup.electrum.doubleSpent(specs.head.data.keys.ewt.xPub, info.tx)
        if (sanityCheck.depth > 0 || sanityCheck.isDoubleSpent) return

        val cpfpBumpOrder = SemanticOrder(info.txid.toHex, System.currentTimeMillis)
        // Only update parent semantic order if it does not already have one, record it BEFORE sending CPFP
        val parentDescWithOrder = info.description.withNewOrderCond(cpfpBumpOrder.copy(order = Long.MinValue).asSome)
        currentGroup.txDataBag.updDescription(parentDescWithOrder, info.txid)

        runInFutureProcessOnUI(currentGroup.electrum.makeCPFP(specs, fromOutPoints.toSet, ourPubKeyScript, feeView.rate), onFail) { response =>
          val desc = CoinDescription(address :: Nil, label = None, currentGroup.netId, cpfpBumpOrder.asSome, cpfpBy = None, cpfpOf = info.txid.asSome)
          alert.dismiss

          runFutureProcessOnUI(broadcast(currentGroup, desc, response.tx, response.transferred, sent = Satoshi(0L), response.fee, incoming = 1), onFail) {
            case None => currentGroup.txDataBag.updDescription(parentDescWithOrder.withNewCPFPBy(response.tx.txid), info.txid)
            case Some(error) => cleanFailedBtcBroadcast(currentGroup, info, response.tx.txid.toHex, error.message)
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

    def boostRBF(info: CoinDetails): Unit = info.extPubs.flatMap(currentGroup.electrum.specs.get) match {
      case specs if specs.size < info.extPubs.size => WalletApp.app.quickToast(error_no_wallet)
      case specs if info.description.taRoi.isDefined => doBoostRBF(specs, info, txSendProxyTa)
      case specs => doBoostRBF(specs, info, txToSendProxyNoop)
    }

    def doBoostRBF(specs: Seq[WalletSpec], info: CoinDetails, txToSendProxy: TxToSendProxy): Unit = {
      val currentFee = CoinDenom.parsedTT(info.feeSat.toMilliSatoshi, cardIn, cardZero)
      val changeTo = ElectrumWallet.orderByImportance(candidates = specs).head

      val sendView = new BtcSendView(currentGroup.fiatRates, specs, MAX_MSAT)
      val blockTarget = currentGroup.feeRates.info.onChainFeeConf.feeTargets.fundingBlockTarget
      val target = currentGroup.feeRates.info.onChainFeeConf.feeEstimator.getFeeratePerKw(blockTarget)
      lazy val feeView = new FeeView[RBFResponse](currentGroup.fiatRates, FeeratePerByte(target), sendView.rbfView.fvc) {
        rate = target

        worker = new ThrottledWork[String, RBFResponse] {
          override def process(reason: String, response: RBFResponse): Unit = response.result match {
            case Left(ElectrumWallet.PARENTS_MISSING) => showRbfErrorDesc(rbf_err_parents_missing)
            case Left(ElectrumWallet.FOREIGN_INPUTS) => showRbfErrorDesc(rbf_err_foreign_inputs)
            case Right(res) => update(res.fee.toMilliSatoshi.asSome, showIssue = false)
            case _ => error(new RuntimeException)
          }

          override def work(reason: String): RBFResponse = currentGroup.electrum.rbfBump(specs, changeTo, info.tx, rate)
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
        val sanityCheck = currentGroup.electrum.doubleSpent(specs.head.data.keys.ewt.xPub, info.tx)
        if (sanityCheck.depth > 0 || sanityCheck.isDoubleSpent) return

        val rbfParams = RBFParams(info.txid, CoinDescription.RBF_BOOST)
        val ofOriginalTxid = info.description.rbf.map(_.ofTxid).getOrElse(info.txid)
        val rbfBumpOrder = SemanticOrder(ofOriginalTxid.toHex, -System.currentTimeMillis)

        def proceedBroadcastWithoutConfirm(response: GenerateTxResponse): Unit = runAnd(alert1.dismiss) {
          val desc = CoinDescription(Nil, None, currentGroup.netId, rbfBumpOrder.asSome, None, None, rbfParams.asSome, info.description.taRoi)
          runFutureProcessOnUI(broadcast(currentGroup, desc, response.tx, received = Satoshi(0L), info.sentSat, response.fee, incoming = 0), onFail) {
            case None => currentGroup.txDataBag.updDescription(info.description.withNewOrderCond(rbfBumpOrder.copy(order = Long.MaxValue).asSome), info.txid)
            case Some(error) => cleanFailedBtcBroadcast(currentGroup, info, response.tx.txid.toHex, error.message)
          }
        }

        sendView.setInputEnabled(alert1, isEnabled = false).run
        val proceed: ResponseToNothing = txToSendProxy(proceedBroadcastWithoutConfirm, sendView, alert1)
        runInFutureProcessOnUI(currentGroup.electrum.rbfBump(specs, changeTo, info.tx, feeView.rate).result.right.get, onFail)(proceed)
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

    def cancelRBF(info: CoinDetails): Unit = info.extPubs.flatMap(currentGroup.electrum.specs.get) match {
      case Nil => WalletApp.app.quickToast(error_no_wallet) case specs => doCancelRBF(specs, info)
    }

    def doCancelRBF(specs: Seq[WalletSpec], info: CoinDetails): Unit = {
      val changeSpec = ElectrumWallet.orderByImportance(candidates = specs).head
      val address = changeSpec.data.keys.ewt.textAddress(changeSpec.data.changePubKey)
      val ourPubKeyScript = currentGroup.electrum.addressToPubKeyScript(address)

      val sendView = new BtcSendView(currentGroup.fiatRates, specs, MAX_MSAT)
      val currentFee = CoinDenom.parsedTT(info.feeSat.toMilliSatoshi, cardIn, cardZero)
      val blockTarget = currentGroup.feeRates.info.onChainFeeConf.feeTargets.fundingBlockTarget
      val target = currentGroup.feeRates.info.onChainFeeConf.feeEstimator.getFeeratePerKw(blockTarget)
      lazy val feeView = new FeeView[RBFResponse](currentGroup.fiatRates, FeeratePerByte(target), sendView.rbfView.fvc) {
        rate = target

        worker = new ThrottledWork[String, RBFResponse] {
          override def process(reason: String, response: RBFResponse): Unit = response.result match {
            case Left(ElectrumWallet.PARENTS_MISSING) => showRbfErrorDesc(rbf_err_parents_missing)
            case Left(ElectrumWallet.FOREIGN_INPUTS) => showRbfErrorDesc(rbf_err_foreign_inputs)
            case Right(res) => update(res.fee.toMilliSatoshi.asSome, showIssue = false)
            case _ => error(new RuntimeException)
          }

          override def work(reason: String): RBFResponse = currentGroup.electrum.rbfReroute(specs, info.tx, rate, ourPubKeyScript)
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
        val sanityCheck = currentGroup.electrum.doubleSpent(specs.head.data.keys.ewt.xPub, info.tx)
        if (sanityCheck.depth > 0 || sanityCheck.isDoubleSpent) return

        val rbfParams = RBFParams(info.txid, CoinDescription.RBF_CANCEL)
        val ofOriginalTxid = info.description.rbf.map(_.ofTxid).getOrElse(info.txid).toHex
        val rbfBumpOrder = SemanticOrder(ofOriginalTxid, -System.currentTimeMillis)

        runInFutureProcessOnUI(currentGroup.electrum.rbfReroute(specs, info.tx, feeView.rate, ourPubKeyScript).result.right.get, onFail) { response =>
          val desc = CoinDescription(addresses = Nil, label = None, currentGroup.netId, rbfBumpOrder.asSome, cpfpBy = None, cpfpOf = None, rbfParams.asSome)
          alert.dismiss

          runFutureProcessOnUI(broadcast(currentGroup, desc, response.tx, info.sentSat - response.fee, sent = 0L.sat, response.fee, incoming = 1), onFail) {
            case None => currentGroup.txDataBag.updDescription(info.description.withNewOrderCond(rbfBumpOrder.copy(order = Long.MaxValue).asSome), info.txid)
            case Some(error) => cleanFailedBtcBroadcast(currentGroup, info, response.tx.txid.toHex, error.message)
          }
        }
      }

      lazy val alert = {
        val title = getString(rbf_cancel_explain).asDefView
        val builder = titleBodyAsViewBuilder(title, sendView.body)
        mkCheckForm(attempt, none, builder, dialog_ok, dialog_cancel)
      }

      sendView.rbfView.rbfCurrent.secondItem.setText(currentFee.html)
      feeView.update(feeOpt = Option.empty, showIssue = false)
      feeView.fvc.customFeerateOption.performClick
      sendView.defaultView = sendView.rbfView
      sendView.switchToDefault(alert)
    }

    // VIEW RELATED

    def collapse: Unit = {
      openListItems -= currentDetails.identity
      setVis(isVisible = false, extraInfo)
      extraInfo.removeAllViewsInLayout
    }

    def expand: Unit = {
      openListItems += currentDetails.identity
      setVis(isVisible = true, extraInfo)
      extraInfo.removeAllViewsInLayout

      currentDetails match {
        case info: CoinDetails =>
          val isRbfCancel = info.description.rbf.exists(_.mode == CoinDescription.RBF_CANCEL)
          val canRBF = !info.isIncoming && !info.isDoubleSpent && !info.isConfirmed && info.description.cpfpOf.isEmpty
          val canCPFP = info.isIncoming && !info.isDoubleSpent && !info.isConfirmed && info.description.rbf.isEmpty && info.description.canBeCPFPd
          val fee = CoinDenom.parsed(info.feeSat.toMilliSatoshi, cardIn, cardZero)

          for (btcLabel <- info.description.label) addFlowChip(extraInfo, btcLabel, R.drawable.border_white, None)
          addFlowChip(extraInfo, getString(popup_txid).format(info.identity.short), R.drawable.border_gray)(me browse s"https://mempool.space/tx/${info.identity}")
          if (!info.isIncoming || isRbfCancel || info.description.cpfpOf.isDefined) addFlowChip(extraInfo, getString(popup_fee).format(fee), R.drawable.border_gray)(none)

          if (canRBF) addFlowChip(extraInfo, getString(dialog_boost), R.drawable.border_white)(self boostRBF info)
          if (canRBF) addFlowChip(extraInfo, getString(dialog_cancel), R.drawable.border_white)(self cancelRBF info)
          if (canCPFP) addFlowChip(extraInfo, getString(dialog_boost), R.drawable.border_white)(self boostCPFP info)
      }
    }

    def updateDetails(details: ItemDetails): Unit = {
      currentGroup = details.description.networkId match {
        case WalletApp.ID_BTC => WalletApp.btc
        case WalletApp.ID_ECX => WalletApp.ecx
        case _ => throw new Exception
      }

      currentDetails = details
      if (openListItems contains currentDetails.identity) expand else collapse
      meta setText WalletApp.when(currentDetails.date, WalletApp.app.dateFormat)

      currentDetails match {
        case info: CoinDetails =>
          // Ephemeral tx has no connected wallet while it's being broadcasted
          // User may remove a wallet while related transactions are getting confirmed
          val hasNoWallets = info.extPubs.flatMap(currentGroup.electrum.specs.get).isEmpty

          if (info.isConfirmed) statusIcon.setImageResource(R.drawable.done_24)
          else if (info.isDoubleSpent) statusIcon.setImageResource(R.drawable.block_24)
          else if (hasNoWallets) statusIcon.setImageResource(R.drawable.question_24)
          else statusIcon.setImageResource(R.drawable.hourglass_empty_24)
      }

      currentDetails match {
        case info: CoinDetails if WalletApp.pendingInfos.contains(info.identity) => itemView.setAlpha(0.6F)
        case _ if currentDetails.isDoubleSpent => itemView.setAlpha(0.6F)
        case _ => itemView.setAlpha(1F)
      }

      currentDetails match {
        case info: CoinDetails if info.description.cpfpOf.isDefined => setVisibleIcon(R.id.inBoosted)
        case info: CoinDetails if info.description.rbf.exists(_.mode == CoinDescription.RBF_BOOST) => setVisibleIcon(R.id.outBoosted)
        case info: CoinDetails if info.description.rbf.exists(_.mode == CoinDescription.RBF_CANCEL) => setVisibleIcon(R.id.outCancelled)
        case info: CoinDetails if info.description.networkId == WalletApp.ID_BTC => setVisibleIcon(R.id.btcInOut)
        case info: CoinDetails if info.description.networkId == WalletApp.ID_ECX => setVisibleIcon(R.id.ecxInOut)
        case _ => throw new Exception
      }

      currentDetails match {
        case info: CoinDetails if info.description.cpfpOf.isDefined => amount.setText(description_cpfp)
        case info: CoinDetails if info.description.rbf.exists(_.mode == CoinDescription.RBF_BOOST) => amount.setText(description_rbf_boost)
        case info: CoinDetails if info.description.rbf.exists(_.mode == CoinDescription.RBF_CANCEL) => amount.setText(description_rbf_cancel)
        case info: CoinDetails => amount.setText(CoinDenom.directedTT(info.receivedSat.toMilliSatoshi, info.sentSat.toMilliSatoshi, cardOut, cardIn, cardZero, info.isIncoming).html)
      }
    }

    def setVisibleIcon(id: Int): Unit = if (lastVisibleIconId != id) {
      iconMap.get(lastVisibleIconId).foreach(_ setVisibility View.GONE)
      iconMap.get(id).foreach(_ setVisibility View.VISIBLE)
      lastVisibleIconId = id
    }
  }

  // LISTENERS

  private var viewUpdateSub = Option.empty[Subscription]
  private var cardsResetSub = Option.empty[Subscription]

  private val chainListener = new WalletEventsListener {
    override def onWalletReady(event: WalletReady): Unit =
      DbStreams.next(DbStreams.txStream)
  }

  private val taErrorListener = new LinkClient.Listener(LinkClient.GENERAL_ERROR) {
    override def onResponse(args: Option[LinkClient.ResponseArguments] = None): Unit = args.foreach {
      case LinkClient.Failure(LinkClient.ACCOUNT_BANNED) => extendedWarn(msgRes = ta_account_disabled).run
      case fail: LinkClient.Failure => UITask(WalletApp.app quickToast fail.failureCode.toString).run
      case _ => // Not interested in anything else
    }

    def extendedWarn(msgRes: Int) = UITask {
      val title = new TitleView(me getString msgRes).asDefView
      val bld = titleBodyAsViewBuilder(null, title).setPositiveButton(dialog_ok, null)
      showForm(bld.create)
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
    try WalletApp.linkClient ! LinkClient.CmdRemove(taErrorListener) catch none
    try WalletApp.btc.electrum.catcher ! WalletEventsCatcher.Remove(chainListener) catch none
    try WalletApp.ecx.electrum.catcher ! WalletEventsCatcher.Remove(chainListener) catch none
    try WalletApp.btc.fiatRates.listeners -= fiatListener catch none
    try WalletApp.ecx.fiatRates.listeners -= fiatListener catch none
    viewUpdateSub.foreach(_.unsubscribe)
    cardsResetSub.foreach(_.unsubscribe)
    super.onDestroy
  }

  override def onRequestPermissionsResult(reqCode: Int, permissions: Array[String], results: Array[Int] = Array.empty): Unit =
    if (reqCode == scannerRequestCode && results.nonEmpty) results.head match {
      case PackageManager.PERMISSION_DENIED if !ActivityCompat.shouldShowRequestPermissionRationale(me, android.Manifest.permission.CAMERA) =>
        val intent = new Intent(android.provider.Settings.ACTION_APPLICATION_DETAILS_SETTINGS, Uri parse s"package:$getPackageName").addFlags(Intent.FLAG_ACTIVITY_NEW_TASK)
        mkCheckForm(alert => runAnd(alert.dismiss)(me startActivity intent), none, titleBodyAsViewBuilder(getString(error_camera_denied).asDefView, null), dialog_ok, dialog_cancel)
      case PackageManager.PERMISSION_DENIED => WalletApp.app.quickToast(error_camera_declined)
      case PackageManager.PERMISSION_GRANTED => bringScanner(null)
    }

  override def checkExternalData(whenNone: Runnable): Unit = InputParser.checkAndMaybeErase {
    case cu: PlainCoinUri if cu.addressGood(WalletApp.btc.electrum) => bringAddressSelector(WalletApp.btc, cu, plainTitle(WalletApp.btc), txToSendProxyNoop).run
    case cu: PlainCoinUri if cu.addressGood(WalletApp.ecx.electrum) => bringAddressSelector(WalletApp.ecx, cu, plainTitle(WalletApp.ecx), txToSendProxyNoop).run
    case a2a: MultiAddressParser.AddressToAmount if a2a.addressGood(WalletApp.btc.electrum) => bringMultiAddressSelector(WalletApp.btc, a2a)
    case a2a: MultiAddressParser.AddressToAmount if a2a.addressGood(WalletApp.ecx.electrum) => bringMultiAddressSelector(WalletApp.ecx, a2a)
    case _ => whenNone.run
  }

  def isSettingsOn: Boolean = walletCards.settingsContainer.getVisibility == View.VISIBLE

  override def START(state: Bundle): Unit =
    WalletApp.isAlive match {
      case true if WalletApp.isOperational =>
        setContentView(R.layout.activity_main)
        WalletApp.btc.electrum.catcher ! chainListener
        WalletApp.ecx.electrum.catcher ! chainListener
        WalletApp.btc.fiatRates.listeners += fiatListener
        WalletApp.ecx.fiatRates.listeners += fiatListener
        WalletApp.linkClient ! taErrorListener

        itemsList.addHeaderView(walletCards.view)
        itemsList.addFooterView(expandContainer)
        itemsList.setAdapter(paymentsAdapter)
        itemsList.setDividerHeight(0)
        itemsList.setDivider(null)

        expand setOnClickListener onButtonTap {
          displayFullIxInfoHistory = !displayFullIxInfoHistory
          expand setRotation { if (displayFullIxInfoHistory) 180F else 0F }
          androidx.transition.TransitionManager.beginDelayedTransition(contentWindow)
          runAnd(loadRecent)(paymentAdapterDataChanged.run)
        }

        walletCards.resetCards
        runAnd(loadRecent)(paymentAdapterDataChanged.run)
        viewUpdateSub = Rx.uniqueFirstAndLastWithinWindow(DbStreams.txStream, 500.millis) {
          // Full view update when it makes sense to load new infos from db (info added, broadcasted, description changed)
          // For BTC txs specifically, we also leverage this opportunity to check whether they got confirmed or double-spent
          loadRecent

          WalletApp.btc.checkConfirms(btcInfosToConsider)
          WalletApp.ecx.checkConfirms(ecxInfosToConsider)
          UITask(walletCards.updateView).run
          paymentAdapterDataChanged.run
        }.asSome

        cardsResetSub = Rx.uniqueFirstAndLastWithinWindow(DbStreams.walletStream, 500.millis) {
          // Full wallet cards reset when it makes sense to reload them from db
          UITask(walletCards.resetCards).run
        }.asSome

      case true =>
        WalletApp.btc.extDataBag.tryGetSecret match {
          case Failure(_: android.database.CursorIndexOutOfBoundsException) =>
            // Record is not present at all, this is probaby a fresh wallet
            me exitTo classOf[SetupActivity]

          case Failure(reason) =>
            // Notify user about it
            throw reason

          case Success(secret) =>
            WalletApp.makeOperational(secret)
            WalletApp.btc.initWallets(WalletApp.secret.keys.bitcoinMaster)
            WalletApp.ecx.initWallets(WalletApp.secret.keys.ecashMaster)
            WalletApp.initTaCard
            START(state)
        }

      case false =>
        WalletApp.btc.makeAlive(WalletApp.app)
        WalletApp.ecx.makeAlive(WalletApp.app)
        START(state)
    }

  // VIEW HANDLERS

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
    androidx.transition.TransitionManager.beginDelayedTransition(walletCards.view)
    setVis(!isSettingsOn, walletCards.settingsContainer)
    walletCards.updateView
  }

  def plainTitle(group: NetworkWalletGroup)(uri: PlainCoinUri): TitleView = {
    val label = uri.label.map(label => s"<br><br><b>$label</b>").getOrElse(new String)
    val caption = getString(dialog_send).format(group.ticker, uri.address.short, label)
    new TitleView(caption)
  }

  def loanTitle(loan: LinkClient.LoanAd): TitleView = {
    val durationHuman = WalletApp.app.plurOrZero(daysLeftRes, loan.durationDays.toInt)
    val parentDuration = getLayoutInflater.inflate(R.layout.frag_two_sided_item_ta, null)
    val parentAPR = getLayoutInflater.inflate(R.layout.frag_two_sided_item_ta, null)

    val titleView = new TitleView(s"<b>${me getString ta_loan} ${WalletApp.btc.ticker}</b>")
    titleView.view.addView(new TwoSidedItem(parentDuration, getString(ta_loan_duration), durationHuman).parent)
    titleView.view.addView(new TwoSidedItem(parentAPR, getString(ta_loan_annual_return), Denomination.formatRoi format loan.roi).parent)
    addFlowChip(titleView.flow, "tactical-advantage.trading", R.drawable.border_white)(me browse "https://tactical-advantage.trading")
    titleView
  }

  def proceedConfirm(group: NetworkWalletGroup, sendView: BtcSendView, desc: CoinDescription, alert: AlertDialog, response: GenerateTxResponse): Unit = {
    sendView.confirmView.confirmFiat.secondItem setText WalletApp.currentMsatInFiatHuman(group.fiatRates, response.transferred.toMilliSatoshi).html
    sendView.confirmView.confirmAmount.secondItem setText CoinDenom.parsedTT(response.transferred.toMilliSatoshi, cardIn, cardZero).html
    sendView.confirmView.confirmFee.secondItem setText CoinDenom.parsedTT(response.fee.toMilliSatoshi, cardIn, cardZero).html
    sendView.confirmView.chainEditButton setOnClickListener onButtonTap(sendView switchToDefault alert)
    sendView.confirmView.chainCancelButton setOnClickListener onButtonTap(alert.dismiss)
    WalletApp.app.hideKeys(sendView.editView.rmc.fiatInputAmount)
    sendView.setButtonsVisible(alert, on = false)
    sendView.switchTo(sendView.confirmView)

    sendView.confirmView.chainNextButton setOnClickListener onButtonTap {
      val broadcastFuture = broadcast(group, desc, response.tx, received = Satoshi(0L), sent = response.transferred, response.fee, incoming = 0)
      runFutureProcessOnUI(broadcastFuture, onFail) { case Some(error) => cleanFailedBroadcast(response.tx.txid.toHex, error.message) case None => }
      alert.dismiss
    }
  }

  def bringAddressSelector[T <: CoinUri](group: NetworkWalletGroup, cu: T, makeTitle: T => TitleView, txToSendProxy: TxToSendProxy) = UITask {
    val pubKeyScript = group.electrum.addressToPubKeyScript(cu.address)

    new WalletSelector(makeTitle(cu), group) {
      def onOk(specs: List[WalletSpec] = Nil): Unit = {
        val sendView = new BtcSendView(group.fiatRates, specs, cu.maxAmount)
        val changeTo = ElectrumWallet.orderByImportance(specs).head

        def attempt(alert1: AlertDialog): Unit = {
          sendView.setInputEnabled(alert1, isEnabled = false).run
          val proceed = txToSendProxy(proceedConfirm(group, sendView, cu.desc.copy(networkId = group.netId), alert1, _: GenerateTxResponse), sendView, alert1)
          runInFutureProcessOnUI(group.electrum.makeTx(specs, changeTo, pubKeyScript, sendView.rm.resultMsat.truncateToSatoshi, Map.empty, feeView.rate), onFail)(proceed)
        }

        lazy val alert = {
          val neutralRes = if (cu.amount.isDefined) -1 else dialog_max
          val builder = titleBodyAsViewBuilder(makeTitle(cu).asColoredView(group.bgRes), sendView.body)
          mkCheckFormNeutral(attempt, none, _ => sendView.rm.updateText(sendView.totalCanSend), builder, dialog_ok, dialog_cancel, neutralRes)
        }

        lazy val feeView = new FeeView[GenerateTxResponse](group.fiatRates, FeeratePerByte(1L.sat), sendView.editView.fvc) {
          val mutualCloseBlockTarget = group.feeRates.info.onChainFeeConf.feeTargets.mutualCloseBlockTarget
          rate = group.feeRates.info.onChainFeeConf.feeEstimator.getFeeratePerKw(mutualCloseBlockTarget)

          worker = new ThrottledWork[String, GenerateTxResponse] {
            // This is a generic sending facility which may send to non-segwit, so always use a safer high dust threshold
            override def work(reason: String): GenerateTxResponse = group.electrum.makeTx(specs, changeTo, pubKeyScript, sendView.rm.resultMsat.truncateToSatoshi, Map.empty, rate)
            override def error(exception: Throwable): Unit = update(feeOpt = None, showIssue = sendView.rm.resultMsat >= group.electrum.params.dustLimit)
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

        cu.amount.foreach { asked =>
          sendView.rm.updateText(value = asked)
          sendView.rm.rmc.inputAmount.setEnabled(false)
          sendView.rm.rmc.fiatInputAmount.setEnabled(false)
        }
      }
    }
  }

  def bringMultiAddressSelector(group: NetworkWalletGroup, a2a: MultiAddressParser.AddressToAmount) = {
    val scriptToAmount = a2a.values.firstItems.map(group.electrum.addressToPubKeyScript).zip(a2a.values.secondItems).toMap
    val titleMsg = getString(dialog_send_many).format(group.ticker)

    new WalletSelector(titleMsg, group) {
      def onOk(specs: List[WalletSpec] = Nil): Unit = {
        val changeTo = ElectrumWallet.orderByImportance(specs).head
        val sendView = new BtcSendView(group.fiatRates, specs, MAX_MSAT)

        def attempt(alert1: AlertDialog): Unit = {
          val desc = CoinDescription(a2a.values.firstItems.toList, None, group.netId)
          val proceed = proceedConfirm(group, sendView, desc, alert1, _: GenerateTxResponse)
          runInFutureProcessOnUI(group.electrum.makeBatchTx(specs, changeTo, scriptToAmount, feeView.rate), onFail)(proceed)
        }

        lazy val alert = {
          val view = new TitleView(titleMsg).asColoredView(group.bgRes)
          mkCheckForm(attempt, none, titleBodyAsViewBuilder(view, sendView.body),
            dialog_ok, dialog_cancel)
        }

        lazy val feeView = new FeeView[GenerateTxResponse](group.fiatRates, FeeratePerByte(1L.sat), sendView.editView.fvc) {
          val mutualCloseBlockTarget = group.feeRates.info.onChainFeeConf.feeTargets.mutualCloseBlockTarget
          rate = group.feeRates.info.onChainFeeConf.feeEstimator.getFeeratePerKw(mutualCloseBlockTarget)

          worker = new ThrottledWork[String, GenerateTxResponse] {
            override def work(reason: String): GenerateTxResponse = group.electrum.makeBatchTx(specs, changeTo, scriptToAmount, rate)
            override def process(reason: String, response: GenerateTxResponse): Unit = update(response.fee.toMilliSatoshi.asSome, showIssue = false)
            override def error(exception: Throwable): Unit = update(feeOpt = None, showIssue = true)
          }

          override def update(feeOpt: Option[MilliSatoshi], showIssue: Boolean): Unit = UITask {
            updatePosButton(alert, isEnabled = feeOpt.isDefined).run
            super.update(feeOpt, showIssue)
          }.run
        }

        for (address \ amount <- a2a.values.reverse) {
          val humanAmount = CoinDenom.parsedTT(amount.toMilliSatoshi, cardIn, cardZero)
          val parent = getLayoutInflater.inflate(R.layout.frag_two_sided_item_gen, null)
          new TwoSidedItem(parent, address.short.html, humanAmount.html)
          sendView.editChain.addView(parent, 0)
        }

        setVis(isVisible = false, sendView.inputChain)
        feeView.update(feeOpt = None, showIssue = false)
        feeView.worker addWork "MULTI-SEND-INIT-CALL"
      }
    }
  }

  def bringTaSignInDialogEmail(titleMsg: String): Unit = {
    val (builder, extraInputLayout, extraInputField) = singleInputPopupBuilder(new TitleView(titleMsg).asDefView)
    lazy val alert = mkCheckForm(proceed, none, builder, dialog_ok, dialog_cancel)

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

    def onText(inputText: String) =
      updatePosButton(alert, inputText.nonEmpty).run

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
    val expandVisible = btcInfosToConsider.size + ecxInfosToConsider.size > DEFAULT_SHOW_ITEMS
    setVisMany(expandVisible -> expandContainer, allInfos.nonEmpty -> walletCards.recentActivity)
    paymentsAdapter.notifyDataSetChanged
  }

  def broadcast(group: NetworkWalletGroup, desc: CoinDescription, finalTx: Transaction, received: Satoshi, sent: Satoshi, fee: Satoshi, incoming: Int): Future[OkOrError] = {
    val info = CoinDetails(finalTx.toString, finalTx.txid.toHex, invalidPubKey.toString, depth = 0L, received, sent, fee, seenAt = System.currentTimeMillis,
      updatedAt = System.currentTimeMillis, desc, 0L.msat, group.fiatRates.info.rates.toJson.compactPrint, incoming, doubleSpent = 0L)
    WalletApp.pendingInfos(info.identity) = info
    WalletApp.seenInfos(info.identity) = info
    DbStreams.next(DbStreams.txStream)
    group.electrum.broadcast(finalTx)
  }

  def cleanFailedBtcBroadcast(group: NetworkWalletGroup, info: CoinDetails, failedKey: String, message: String): Unit = {
    group.txDataBag.updDescription(info.description, info.txid)
    cleanFailedBroadcast(failedKey, message)
  }

  def cleanFailedBroadcast(failedKey: String, message: String): Unit = {
    WalletApp.pendingInfos.remove(failedKey)
    WalletApp.seenInfos.remove(failedKey)
    DbStreams.next(DbStreams.txStream)
    onFail(message)
  }

  // WALLET CARDS

  abstract class WalletSelector(title: TitleView, group: NetworkWalletGroup) { self =>
    val spendable = group.electrum.specs.values.filter(_.spendable).toList
    val usable = group.electrum.specs.values.filter(_.usable).toList
    def onOk(specs: List[WalletSpec] = Nil): Unit

    if (group.electrum.specs.isEmpty) {
      WalletApp.app.quickToast(error_no_wallet)
    } else if (spendable.size == 1) onOk(spendable)
    else if (usable.size == 1) onOk(usable)
    else {
      val info = addFlowChip(title.flow, getString(select_wallets), R.drawable.border_white, None)
      val cardsContainer = getLayoutInflater.inflate(R.layout.frag_linear_layout, null).asInstanceOf[LinearLayout]
      lazy val alert = mkCheckForm(proceed, none, titleBodyAsViewBuilder(title.view, cardsContainer), dialog_ok, dialog_cancel)

      def proceed(alert1: AlertDialog): Unit = runAnd(alert1.dismiss)(self onOk chosenSpecs.toList)
      def chosenSpecs: Seq[WalletSpec] = cards.filter(_.isSelected).map(_.xPub).flatMap(group.electrum.specs.get)

      lazy val cards =
        for (spec <- spendable) yield
          new CoinWalletCard(spec.data.keys.ewt.xPub, group) {
            setVisMany(false -> cardButtons, false -> infoWalletNotice)

            def onTap: Unit = {
              isSelected = !isSelected
              val totalCanSend = chosenSpecs.map(_.info.lastBalance).sum.toMilliSatoshi
              val formatted = "<b>sum</b> " + CoinDenom.parsedTT(totalCanSend, cardIn, cardZero)
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
    addFlowChip(cardButtons, getString(dialog_hide), R.drawable.border_white)(hide)

    def hide: Unit = none
    def updateView: Unit
    def onTap: Unit
  }

  abstract class CoinWalletCard(val xPub: ExtendedPublicKey, group: NetworkWalletGroup) extends WalletCard {
    imageTip setImageResource R.drawable.add_24
    var isSelected: Boolean = false

    def updateView: Unit = {
      val spec = group.electrum.specs(xPub)
      val hasMoney = spec.info.lastBalance.toLong > 0L
      val bgResource = if (isSelected) group.bgSelectedRes else group.bgRes
      val attachment = if (spec.info.core.attachedMaster.isDefined) R.drawable.attachment_24 else 0
      infoWalletLabel setText spec.info.label.asSome.filter(_.trim.nonEmpty).getOrElse(group.ticker)
      balanceWallet setText CoinDenom.parsedTT(spec.info.lastBalance.toMilliSatoshi, "#FFFFFF", group.zeroColor).html
      balanceWalletFiat setText WalletApp.currentMsatInFiatHuman(group.fiatRates, spec.info.lastBalance.toMilliSatoshi)
      infoWalletLabel.setCompoundDrawablesWithIntrinsicBounds(0, 0, attachment, 0)
      setVisMany(hasMoney -> balanceContainer, !hasMoney -> imageTip)
      infoContainer setBackgroundResource bgResource
    }
  }

  abstract class TaWalletCard(parent: WalletCardsViewHolder) extends WalletCard {
    val earnAccount = new ExpandedEarnAccount
    cardView.addView(earnAccount.wrap, 0)
    infoWalletLabel setText ta_earn_label

    def updateView: Unit =
      WalletApp.linkClient.data match {
        case status: LinkClient.UserStatus =>
          infoWalletNotice setText status.email
          imageTip.setImageResource(R.drawable.info_24)
          balanceWalletFiat setText WalletApp.app.plurOrZero(daysLeftRes, status.minLoanDaysLeft)
          balanceWallet setText WalletApp.app.plurOrZero(activeLoansRes, status.activeLoans.size)
          setVisMany(parent.isEarnAccountExpanded -> earnAccount.wrap, !parent.isEarnAccountExpanded -> infoContainer)
          setVisMany(status.activeLoans.nonEmpty -> balanceContainer, status.activeLoans.isEmpty -> imageTip)
          earnAccount.updateView(status)
        case LinkClient.LoggedOut if parent.isEarnAccountExpanded =>
          setVisMany(false -> earnAccount.wrap, true -> infoContainer)
          parent.isEarnAccountExpanded = false
          updateView
        case LinkClient.LoggedOut =>
          infoWalletNotice setText ta_client_login
          imageTip.setImageResource(R.drawable.lock_24)
          setVis(isVisible = false, balanceContainer)
          setVis(isVisible = true, imageTip)
      }
  }

  class ExpandedEarnAccount {
    val wrap: LinearLayout = getLayoutInflater.inflate(R.layout.frag_ta_account, null).asInstanceOf[LinearLayout]
    val taBalancesContainer: LinearLayout = wrap.findViewById(R.id.taBalancesContainer).asInstanceOf[LinearLayout]
    val taLoansContainer: LinearLayout = wrap.findViewById(R.id.taLoansContainer).asInstanceOf[LinearLayout]
    val taClientEmail: TextView = wrap.findViewById(R.id.taClientEmail).asInstanceOf[TextView]
    val taExtended: FlowLayout = wrap.findViewById(R.id.taExtended).asInstanceOf[FlowLayout]
    val taBalancesTitle: View = wrap.findViewById(R.id.taBalancesTitle).asInstanceOf[View]
    val taLoansTitle: View = wrap.findViewById(R.id.taLoansTitle).asInstanceOf[View]
    val taInfo: TextView = wrap.findViewById(R.id.taInfo).asInstanceOf[TextView]

    def updateView(status: LinkClient.UserStatus): Unit = {
      List(taBalancesContainer, taLoansContainer, taExtended).foreach(_.removeAllViewsInLayout)
      setVisMany(status.totalFunds.nonEmpty -> taBalancesTitle, status.activeLoans.nonEmpty -> taLoansTitle)
      taClientEmail.setText(status.email)

      for (total <- status.totalFunds) {
        val amount = Btc(total.withdrawable).toSatoshi.toMilliSatoshi
        val parent = getLayoutInflater.inflate(R.layout.frag_two_sided_item_ta, null)
        val item = new TwoSidedItem(parent, WalletApp.btc.ticker, CoinDenom.parsedTT(amount, cardIn, cardZero).html)
        item.firstItem.setCompoundDrawablesWithIntrinsicBounds(R.drawable.ic_logo_bitcoin_24, 0, 0, 0)
        taBalancesContainer.addView(parent)
      }

      for (loan <- status.activeLoans) {
        val daysLeft = WalletApp.app.plurOrZero(daysLeftRes, loan.daysLeft.toInt)
        val parent = getLayoutInflater.inflate(R.layout.frag_two_sided_item_ta, null)
        val amount = CoinDenom.parsedTT(Btc(loan.amount).toSatoshi.toMilliSatoshi, cardIn, cardZero)
        val interest = CoinDenom.directedTT(Btc(loan.interest).toSatoshi.toMilliSatoshi, MilliSatoshi(0L), cardOut, cardIn, cardZero, isIncoming = true)
        val item = new TwoSidedItem(parent, s"APR ${Denomination.formatRoi format loan.roi}<br><small><tt>$daysLeft</tt></small>".html, s"$amount<br><small>$interest</small>".html)
        item.firstItem.setCompoundDrawablesWithIntrinsicBounds(R.drawable.ic_logo_bitcoin_24, 0, 0, 0)
        taLoansContainer.addView(parent)
      }

      if (status.pendingDeposits.nonEmpty) {
        taInfo setText getString(ta_pending_deposit)
        setVis(isVisible = true, view = taInfo)
      } else if (status.pendingWithdraws.nonEmpty) {
        val humanDate = WalletApp.when(status.withdrawDate, WalletApp.app.dateFormat)
        taInfo setText getString(status.pendingWithdraws.head.nextWithdrawRes).format(humanDate)
        setVis(isVisible = true, view = taInfo)
      }

      lazy val callWithdraw: LinkClient.RequestArguments => Unit = { request =>
        val withdrawListener = new LinkClient.Listener("withdraw-request") { self =>
          override def onResponse(args: Option[LinkClient.ResponseArguments] = None): Unit =
            onDisconnected

          override def onDisconnected: Unit = {
            withdrawButtonOpt.foreach(updateViewEnabled(_, isEnabled = true).run)
            WalletApp.linkClient ! LinkClient.CmdRemove(self)
          }
        }

        withdrawButtonOpt.foreach(updateViewEnabled(_, isEnabled = false).run)
        WalletApp.linkClient ! LinkClient.Request(request, withdrawListener.id)
        WalletApp.linkClient ! withdrawListener
      }

      lazy val withdrawButtonOpt = (status.totalFunds.nonEmpty, status.pendingWithdraws.isEmpty) match {
        case (true, true) => addFlowChip(taExtended, getString(ta_withdraw_on), R.drawable.border_white)(requestWithdraw).asSome
        case (true, false) => addFlowChip(taExtended, getString(ta_withdraw_off), R.drawable.border_white)(callWithdraw apply LinkClient.CancelWithdraw).asSome
        case _ => None
      }

      def requestWithdraw: Unit = {
        val doRequestWithdraw: Int => Unit = pos =>
          WalletApp.btc.electrum.specs.values.find(btcSpec => btcSpec.data.keys.ewt.secrets.nonEmpty && btcSpec.info.core.attachedMaster.isEmpty) match {
            case Some(spec) if pos == 1 => callWithdraw apply LinkClient.WithdrawReq(spec.data.keys.ewt.textAddress(spec.data.keys.accountKeys.head), PartialInterestNative)
            case Some(spec) => callWithdraw apply LinkClient.WithdrawReq(spec.data.keys.ewt.textAddress(spec.data.keys.accountKeys.head), FullBalance)
            case None => WalletApp.app.quickToast(error_no_wallet)
          }

        val listOptions = List(ta_withdraw_full, ta_withdraw_interest).map(getString)
        val list = getLayoutInflater.inflate(R.layout.frag_selector_list, null).asInstanceOf[ListView]
        list setAdapter new ArrayAdapter(me, android.R.layout.simple_expandable_list_item_1, listOptions.toArray)
        new sheets.ChoiceBottomSheet(list, doRequestWithdraw).show(getSupportFragmentManager, "unused-tag")
      }

      lazy val getLoanAdButton: TextView = addFlowChip(taExtended, getString(ta_loan), R.drawable.border_white) {
        WalletApp.linkClient ! LinkClient.Request(LinkClient.GetLoanAd, loanAdListener.id)
        updateViewEnabled(getLoanAdButton, isEnabled = false).run
        WalletApp.linkClient ! loanAdListener
      }

      lazy val loanAdListener = new LinkClient.Listener("get-loan-ad") {
        override def onResponse(args: Option[LinkClient.ResponseArguments] = None): Unit = {
          args.collectFirst { case data: LinkClient.LoanAd => bringAddressSelector(WalletApp.btc, data, loanTitle, txSendProxyTa).run }
          onDisconnected
        }

        override def onDisconnected: Unit = {
          updateViewEnabled(getLoanAdButton, isEnabled = true).run
          WalletApp.linkClient ! LinkClient.CmdRemove(this)
        }
      }

      getLoanAdButton
      withdrawButtonOpt

      addFlowChip(taExtended, getString(ta_support), R.drawable.border_white)(me browse "mailto:contact@tactical-advantage.trading")
      addFlowChip(taExtended, getString(ta_logout), R.drawable.border_white)(WalletApp.linkClient ! LinkClient.LoggedOut)
    }
  }

  class WalletCardsViewHolder {
    var isEarnAccountExpanded = false
    val view = getLayoutInflater.inflate(R.layout.frag_wallet_cards, null).asInstanceOf[LinearLayout]
    val fiatUnitPriceAndChange = view.findViewById(R.id.fiatUnitPriceAndChange).asInstanceOf[TextView]
    val holder = view.findViewById(R.id.chainCardsContainer).asInstanceOf[LinearLayout]
    val recentActivity = view.findViewById(R.id.recentActivity).asInstanceOf[View]
    val manager = new WalletCardManager(holder)

    // Settings region
    val settingsContainer = view.findViewById(R.id.settingsContainer).asInstanceOf[LinearLayout]
    val devInfo = me clickableTextField settingsContainer.findViewById(R.id.devInfo).asInstanceOf[TextView]
    val settingsButtons = settingsContainer.findViewById(R.id.settingsButtons).asInstanceOf[FlowLayout]
    val nameAndVer = settingsContainer.findViewById(R.id.nameAndVer).asInstanceOf[TextView]
    val appName = s"${me getString app_name} <font color=$cardZero>v3.3-8</font>"
    val coin = 100000000000L.msat

    devInfo.setText(getString(dev_info).html)
    nameAndVer.setText(appName.html)

    def showWalletCard(group: NetworkWalletGroup, master: ExtendedPrivateKey) = {
      val nativeSpec = group.createWallet(ord = 0L, master)
      group.postInitWallet(nativeSpec)
    }

    def showTaCard = {
      WalletApp.setShowTaCard(show = true)
      DbStreams.next(DbStreams.walletStream)
      WalletApp.initTaCard
    }

    def attachWallet = showMnemonicInput(action_recovery_phrase_title) { mnemonic =>
      val attachedKeys = MasterKeys.fromSeed(MnemonicCode.toSeed(mnemonic, new String).toArray)
      WalletApp.btc.attachWallet(attachedKeys.bitcoinMaster)
      WalletApp.ecx.attachWallet(attachedKeys.ecashMaster)
    }

    def makeCards = {
      lazy val taClientCard = new TaWalletCard(this) {
        override def onTap: Unit = WalletApp.linkClient.data match {
          case LinkClient.LoggedOut => bringTaSignInDialogEmail(me getString ta_login_title_email)
          case _ => runAnd(isEarnAccountExpanded = !isEarnAccountExpanded)(this.updateView)
        }

        override def hide: Unit = {
          WalletApp.setShowTaCard(show = false)
          DbStreams.next(DbStreams.walletStream)
        }
      }

      val coinCards = for {
        group <- List(WalletApp.btc, WalletApp.ecx)
        xPub <- group.electrum.specs.keys.toList
        dest = classOf[QRCoinActivity]
      } yield new CoinWalletCard(xPub, group) {
        override def onTap: Unit = goToWithValue(dest, group -> xPub)
        override def hide: Unit = group.removeWallet(key = xPub)
      }

      if (!WalletApp.getShowTaCard) coinCards
      else coinCards :+ taClientCard
    }

    def resetCards: Unit = {
      holder.removeAllViewsInLayout
      manager.init(makeCards)
      updateView
    }

    def updateView: Unit = {
      val change = WalletApp.btc.fiatRates.info.pctDifference(WalletApp.fiatCode).getOrElse(new String)
      val unitRate = WalletApp.msatInFiatHuman(WalletApp.btc.fiatRates, WalletApp.fiatCode, coin, Denomination.formatFiatShort)
      fiatUnitPriceAndChange.setText(s"BTC &middot; $unitRate $change".html)
      manager.cardViews.foreach(_.updateView)

      settingsButtons.removeAllViewsInLayout
      setVis(isVisible = isSettingsOn, view = settingsButtons)
      for (view <- walletCards.manager.cardViews) setVis(isSettingsOn, view.cardButtons)

      if (isSettingsOn) {
        val msg = getString(settings_show)
        val hasNativeBtc = WalletApp.btc.electrum.specs.values.exists(_.info.core.attachedMaster.isEmpty)
        if (!hasNativeBtc) addFlowChip(settingsButtons, msg.format(WalletApp.btc.ticker), R.drawable.border_white) {
          showWalletCard(WalletApp.btc, WalletApp.secret.keys.bitcoinMaster)
        }

        val hasNativeEcx = WalletApp.ecx.electrum.specs.values.exists(_.info.core.attachedMaster.isEmpty)
        if (!hasNativeEcx) addFlowChip(settingsButtons, msg.format(WalletApp.ecx.ticker), R.drawable.border_white) {
          showWalletCard(WalletApp.ecx, WalletApp.secret.keys.ecashMaster)
        }

        if (!WalletApp.getShowTaCard) addFlowChip(settingsButtons, getString(settings_show_ta), R.drawable.border_white)(showTaCard)
        addFlowChip(settingsButtons, getString(settings_view_recovery_phrase), R.drawable.border_white)(viewRecoveryCode)
        addFlowChip(settingsButtons, getString(settings_attach_wallet), R.drawable.border_white)(attachWallet)
      }
    }
  }
}