package finance.valet

import java.net.InetSocketAddress
import java.text.{DecimalFormat, SimpleDateFormat}
import java.util.Date
import akka.actor.Props
import android.net.Uri
import android.app.{Application, NotificationChannel, NotificationManager}
import android.content._
import android.os.{Build, VibrationEffect}
import android.text.format.DateFormat
import android.view.inputmethod.InputMethodManager
import android.widget.{EditText, Toast}
import androidx.appcompat.app.AppCompatDelegate
import androidx.multidex.MultiDex
import BaseActivity.StringOps
import finance.valet.utils.{AwaitService, LocalBackup}
import finance.valet.sqlite.DBInterfaceSQLiteAndroidMisc
import finance.valet.utils.DelayedNotification
import finance.valet.R.string._
import finance.valet.sqlite._
import com.guardanis.applock.AppLock
import com.softwaremill.quicklens._
import fr.acinq.bitcoin.{Block, ByteVector32, Satoshi, SatoshiLong}
import fr.acinq.eclair._
import fr.acinq.eclair.blockchain.electrum.ElectrumClient.SSL
import fr.acinq.eclair.blockchain.electrum.ElectrumClientPool.ElectrumServerAddress
import fr.acinq.eclair.blockchain.electrum.ElectrumWallet.{TransactionReceived, WalletReady}
import fr.acinq.eclair.blockchain.electrum._
import fr.acinq.eclair.blockchain.electrum.db.{CompleteChainWalletInfo, SigningWallet, WatchingWallet}
import fr.acinq.eclair.blockchain.{CurrentBlockCount, EclairWallet}
import fr.acinq.eclair.channel.{CMD_CHECK_FEERATE, NormalCommits, PersistentChannelData}
import fr.acinq.eclair.router.Router.RouterConf
import fr.acinq.eclair.wire.CommonCodecs.nodeaddress
import fr.acinq.eclair.wire.NodeAddress
import immortan._
import immortan.crypto.Tools._
import immortan.sqlite._
import immortan.utils._
import rx.lang.scala.Observable
import scodec.bits.BitVector

import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.concurrent.duration._
import scala.util.Try


object WalletApp {
  var chainWalletBag: SQLiteChainWallet = _
  var extDataBag: SQLiteDataExtended = _
  var lnUrlPayBag: SQLiteLNUrlPay = _
  var txDataBag: SQLiteTx = _
  var app: WalletApp = _

  val txDescriptions = mutable.Map.empty[ByteVector32, TxDescription]
  var currentChainNode: Option[InetSocketAddress] = None

  final val dbFileNameMisc = "misc.db"
  final val dbFileNameGraph = "graph.db"
  final val dbFileNameEssential = "essential.db"

  val backupSaveWorker: ThrottledWork[Boolean, Any] = new ThrottledWork[Boolean, Any] {
    private def doAttemptStore: Unit = {
      LocalBackup.encryptAndWritePlainBackup(app, dbFileNameEssential, LNParams.chainHash, LNParams.secret.seed)
      // Uncommented only in development builds
      // LocalBackup.writeCompressedGraph(app, dbFileNameGraph, LNParams.chainHash)
    }
    def process(useDelay: Boolean, unitAfterDelay: Any): Unit = if (LocalBackup isAllowed app) try doAttemptStore catch none
    def work(useDelay: Boolean): Observable[Any] = if (useDelay) Rx.ioQueue.delay(4.seconds) else Observable.just(null)
  }

  final val FIAT_CODE = "fiatCode"
  final val BTC_DENOM = "btcDenom"
  final val ENSURE_TOR = "ensureTor"
  final val MAXIMIZED_VIEW = "maximizedView"
  final val LAST_TOTAL_GOSSIP_SYNC = "lastTotalGossipSync"
  final val LAST_NORMAL_GOSSIP_SYNC = "lastNormalGossipSync"
  final val CUSTOM_ELECTRUM_ADDRESS = "customElectrumAddress"
  final val CUSTOM_BACKUP_LOCATION = "customBackupLocation"
  final val SHOW_RATE_US = "showRateUs"

  def useAuth: Boolean = AppLock.isEnrolled(app)
  def fiatCode: String = app.prefs.getString(FIAT_CODE, "usd")
  def ensureTor: Boolean = app.prefs.getBoolean(ENSURE_TOR, false)
  def maximizedView: Boolean = app.prefs.getBoolean(MAXIMIZED_VIEW, true)
  def showRateUs: Boolean = app.prefs.getBoolean(SHOW_RATE_US, true)

  final val CHECKED_BUTTONS = "checkedButtons"
  def getCheckedButtons(default: Set[String] = Set.empty): mutable.Set[String] = app.prefs.getStringSet(CHECKED_BUTTONS, default.asJava).asScala
  def putCheckedButtons(buttons: Set[String] = Set.empty): Unit = app.prefs.edit.putStringSet(CHECKED_BUTTONS, buttons.asJava).commit

  def denom: Denomination = {
    val denom = app.prefs.getString(BTC_DENOM, SatDenomination.sign)
    if (denom == SatDenomination.sign) SatDenomination else BtcDenomination
  }

  def customElectrumAddress: Try[NodeAddress] = Try {
    val rawAddress = app.prefs.getString(CUSTOM_ELECTRUM_ADDRESS, new String)
    nodeaddress.decode(BitVector fromValidHex rawAddress).require.value
  }

  def customBackupLocation: Option[Uri] = {
    val hasIt = app.prefs.contains(CUSTOM_BACKUP_LOCATION)
    if (hasIt) {
      val dir = Try {
        Uri parse app.prefs.getString(WalletApp.CUSTOM_BACKUP_LOCATION, new String)
      }
      dir.toOption
    } else None
  }

  def isAlive: Boolean = null != txDataBag && null != lnUrlPayBag && null != chainWalletBag && null != extDataBag && null != app

  def freePossiblyUsedRuntimeResouces: Unit = {
    // Drop whatever network connections we still have
    CommsTower.workers.values.map(_.pair).foreach(CommsTower.forget)
    // Clear listeners, destroy actors, finalize state machines
    try LNParams.chainWallets.becomeShutDown catch none
    try LNParams.fiatRates.becomeShutDown catch none
    try LNParams.feeRates.becomeShutDown catch none
    try LNParams.cm.becomeShutDown catch none
    // Make non-alive and non-operational`
    LNParams.secret = null
    txDataBag = null
  }

  def restart: Unit = {
    freePossiblyUsedRuntimeResouces
    require(!LNParams.isOperational, "Still operational")
    val intent = new Intent(app, ClassNames.mainActivityClass)
    val restart = Intent.makeRestartActivityTask(intent.getComponent)
    app.startActivity(restart)
    System.exit(0)
  }

  def makeAlive: Unit = {
    // Make application minimally operational (so we can check for seed in db)
    val miscInterface = new DBInterfaceSQLiteAndroidMisc(app, dbFileNameMisc)

    miscInterface txWrap {
      txDataBag = new SQLiteTx(miscInterface)
      lnUrlPayBag = new SQLiteLNUrlPay(miscInterface)
      chainWalletBag = new SQLiteChainWallet(miscInterface)
      extDataBag = new SQLiteDataExtended(miscInterface)
    }

    // In case these are needed early
    LNParams.logBag = new SQLiteLog(miscInterface)
    LNParams.chainHash = Block.Testnet4GenesisBlock.hash
    LNParams.routerConf = RouterConf(initRouteMaxLength = 10, LNParams.maxCltvExpiryDelta)
    LNParams.connectionProvider = if (ensureTor) new TorConnectionProvider(app) else new ClearnetConnectionProvider
    LNParams.ourInit = LNParams.createInit
    LNParams.syncParams = new TestNetSyncParams
    LNParams.ourInit = LNParams.createInit
  }

  def makeOperational(secret: WalletSecret): Unit = {
    require(isAlive, "Application is not alive, hence can not become operational")
    val essentialInterface = new DBInterfaceSQLiteAndroidEssential(app, dbFileNameEssential)
    val graphInterface = new DBInterfaceSQLiteAndroidGraph(app, dbFileNameGraph)
    val currentCustomElectrum: Try[NodeAddress] = customElectrumAddress
    LNParams.secret = secret

    val normalBag = new SQLiteNetwork(graphInterface, NormalChannelUpdateTable, NormalChannelAnnouncementTable, NormalExcludedChannelTable)
    val hostedBag = new SQLiteNetwork(graphInterface, HostedChannelUpdateTable, HostedChannelAnnouncementTable, HostedExcludedChannelTable)
    val payBag = new SQLitePayment(extDataBag.db, preimageDb = essentialInterface)

    val chanBag = new SQLiteChannel(essentialInterface, channelTxFeesDb = extDataBag.db) {
      override def put(data: PersistentChannelData): PersistentChannelData = {
        backupSaveWorker.replaceWork(true)
        super.put(data)
      }
    }

    extDataBag.db txWrap {
      LNParams.feeRates = new FeeRates(extDataBag)
      LNParams.fiatRates = new FiatRates(extDataBag)
      LNParams.fiatRates.focused = Some(fiatCode)
    }

    val pf = new PathFinder(normalBag, hostedBag) {
      override def getLastTotalResyncStamp: Long = app.prefs.getLong(LAST_TOTAL_GOSSIP_SYNC, 0L)
      override def getLastNormalResyncStamp: Long = app.prefs.getLong(LAST_NORMAL_GOSSIP_SYNC, 0L)
      override def updateLastTotalResyncStamp(stamp: Long): Unit = app.prefs.edit.putLong(LAST_TOTAL_GOSSIP_SYNC, stamp).commit
      override def updateLastNormalResyncStamp(stamp: Long): Unit = app.prefs.edit.putLong(LAST_NORMAL_GOSSIP_SYNC, stamp).commit
      override def getExtraNodes: Set[RemoteNodeInfo] = LNParams.cm.all.values.flatMap(Channel.chanAndCommitsOpt).map(_.commits.remoteInfo).toSet
      override def getPHCExtraNodes: Set[RemoteNodeInfo] = LNParams.cm.allHostedCommits.map(_.remoteInfo).toSet
    }

    ElectrumClientPool.loadFromChainHash = {
      case _ if currentCustomElectrum.isSuccess => ElectrumServerAddress(currentCustomElectrum.get.socketAddress, SSL.DECIDE).asSome.toSet
      case Block.LivenetGenesisBlock.hash => ElectrumClientPool.readServerAddresses(app.getAssets open "servers_mainnet.json")
      case Block.TestnetGenesisBlock.hash => ElectrumClientPool.readServerAddresses(app.getAssets open "servers_testnet.json")
      case Block.Testnet4GenesisBlock.hash => ElectrumClientPool.readServerAddresses(app.getAssets open "servers_testnet4.json")
      case _ => throw new RuntimeException
    }

    CheckPoint.loadFromChainHash = {
      case Block.LivenetGenesisBlock.hash => CheckPoint.load(app.getAssets open "checkpoints_mainnet.json")
      case Block.TestnetGenesisBlock.hash => CheckPoint.load(app.getAssets open "checkpoints_testnet.json")
      case Block.Testnet4GenesisBlock.hash => CheckPoint.load(app.getAssets open "checkpoints_testnet4.json")
      case _ => throw new RuntimeException
    }

    LNParams.cm = new ChannelMaster(payBag, chanBag, extDataBag, pf)

    val params = WalletParameters(extDataBag, chainWalletBag, txDataBag, dustLimit = 546L.sat)
    val electrumPool = LNParams.loggedActor(Props(classOf[ElectrumClientPool], LNParams.blockCount, LNParams.chainHash, LNParams.ec), "connection-pool")
    val sync = LNParams.loggedActor(Props(classOf[ElectrumChainSync], electrumPool, params.headerDb, LNParams.chainHash), "chain-sync")
    val watcher = LNParams.loggedActor(Props(classOf[ElectrumWatcher], LNParams.blockCount, electrumPool), "channel-watcher")
    val catcher = LNParams.loggedActor(Props(new WalletEventsCatcher), "events-catcher")

    val walletExt: WalletExt =
      (WalletExt(wallets = Nil, catcher, sync, electrumPool, watcher, params) /: chainWalletBag.listWallets) {
        case ext ~ CompleteChainWalletInfo(core: SigningWallet, persistentSigningWalletData, lastBalance, label, false) =>
          val signingWallet = ext.makeSigningWalletParts(core, lastBalance, label)
          signingWallet.walletRef ! persistentSigningWalletData
          ext.copy(wallets = signingWallet :: ext.wallets)

        case ext ~ CompleteChainWalletInfo(core: WatchingWallet, persistentWatchingWalletData, lastBalance, label, false) =>
          val watchingWallet = ext.makeWatchingWallet84Parts(core, lastBalance, label)
          watchingWallet.walletRef ! persistentWatchingWalletData
          ext.copy(wallets = watchingWallet :: ext.wallets)
      }

    LNParams.chainWallets = if (walletExt.wallets.isEmpty) {
      val defaultLabel = app.getString(R.string.bitcoin_wallet)
      val core = SigningWallet(walletType = EclairWallet.BIP84, isRemovable = false)
      val wallet = walletExt.makeSigningWalletParts(core, Satoshi(0L), defaultLabel)
      walletExt.withFreshWallet(wallet)
    } else walletExt

    LNParams.feeRates.listeners += new FeeRatesListener {
      def onFeeRates(newRatesInfo: FeeRatesInfo): Unit = {
        // We may get fresh feerates after channels become OPEN
        LNParams.cm.all.values.foreach(_ process CMD_CHECK_FEERATE)
        extDataBag.putFeeRatesInfo(newRatesInfo)
      }
    }

    LNParams.fiatRates.listeners += new FiatRatesListener {
      def onFiatRates(newRatesInfo: FiatRatesInfo): Unit =
        extDataBag.putFiatRatesInfo(newRatesInfo)
    }

    // Guaranteed to fire (and update chainWallets) first
    LNParams.chainWallets.catcher ! new WalletEventsListener {
      override def onChainTipKnown(event: CurrentBlockCount): Unit = LNParams.cm.initConnect

      override def onWalletReady(event: WalletReady): Unit = LNParams.synchronized {
        // Wallet is already persisted so our only job at this point is to update runtime
        def sameXPub(wallet: ElectrumEclairWallet): Boolean = wallet.ewt.xPub == event.xPub
        LNParams.chainWallets = LNParams.chainWallets.modify(_.wallets.eachWhere(sameXPub).info) using { info =>
          // Coin control is always disabled on start, we update it later with animation to make it noticeable
          info.copy(lastBalance = event.balance, isCoinControlOn = event.excludedOutPoints.nonEmpty)
        }
      }

      override def onChainMasterSelected(event: InetSocketAddress): Unit = currentChainNode = event.asSome

      override def onChainDisconnected: Unit = currentChainNode = None

      override def onTransactionReceived(event: TransactionReceived): Unit = {
        def addChainTx(received: Satoshi, sent: Satoshi, description: TxDescription, isIncoming: Long): Unit = description match {
          case _: ChanFundingTxDescription => doAddChainTx(received, sent, description, isIncoming, BaseActivity.totalBalance - sent)
          case _ => doAddChainTx(received, sent, description, isIncoming, BaseActivity.totalBalance)
        }

        def doAddChainTx(received: Satoshi, sent: Satoshi, description: TxDescription, isIncoming: Long, totalBalance: MilliSatoshi): Unit = txDataBag.db txWrap {
          txDataBag.addTx(event.tx, event.depth, received, sent, event.feeOpt, event.xPub, description, isIncoming, totalBalance, LNParams.fiatRates.info.rates, event.stamp)
          txDataBag.addSearchableTransaction(description.queryText(event.tx.txid), event.tx.txid)
        }

        val fee = event.feeOpt.getOrElse(0L.sat)
        val defDescription = TxDescription.define(LNParams.cm.all.values, Nil, event.tx)
        val sentTxDescription = txDescriptions.getOrElse(event.tx.txid, default = defDescription)
        if (event.sent == event.received + fee) addChainTx(event.received, event.sent, sentTxDescription, isIncoming = 1L)
        else if (event.sent > event.received) addChainTx(event.received, event.sent - event.received - fee, sentTxDescription, isIncoming = 0L)
        else addChainTx(event.received - event.sent, event.sent, TxDescription.define(LNParams.cm.all.values, event.walletAddreses, event.tx), isIncoming = 1L)
      }
    }

    pf.listeners += LNParams.cm.opm
    // Get channels and still active FSMs up and running
    LNParams.cm.all = Channel.load(Set(LNParams.cm), chanBag)
    // This inital notification will create all in/routed/out FSMs
    LNParams.cm.notifyResolvers

    Rx.repeat(Rx.ioQueue.delay(1.second), Rx.incHour, 1 to Int.MaxValue).foreach { _ =>
      // We need this in case if in/out HTLC is pending for a long time and app is still open
      DelayedNotification.cancel(app, DelayedNotification.IN_FLIGHT_HTLC_TAG)
      if (LNParams.cm.channelsContainHtlc) reScheduleInFlight
    }

    Rx.repeat(Rx.ioQueue.delay(2.seconds), Rx.incHour, 1 to Int.MaxValue).foreach { _ =>
      DelayedNotification.cancel(app, DelayedNotification.WATCH_TOWER_TAG)
      if (vulnerableChannelsExist) reScheduleWatchtower
    }

    LNParams.connectionProvider doWhenReady {
      electrumPool ! ElectrumClientPool.InitConnect
      // Only schedule periodic resync if Lightning channels are being present
      if (LNParams.cm.all.nonEmpty) pf process PathFinder.CMDStartPeriodicResync

      val feeratePeriodHours = 6
      val rateRetry = Rx.retry(Rx.ioQueue.map(_ => LNParams.feeRates.reloadData), Rx.incSec, 3 to 18 by 3)
      val rateRepeat = Rx.repeat(rateRetry, Rx.incHour, feeratePeriodHours to Int.MaxValue by feeratePeriodHours)
      val feerateObs = Rx.initDelay(rateRepeat, LNParams.feeRates.info.stamp, feeratePeriodHours * 3600 * 1000L)
      feerateObs.foreach(LNParams.feeRates.updateInfo, none)

      val fiatPeriodSecs = 60 * 30
      val fiatRetry = Rx.retry(Rx.ioQueue.map(_ => LNParams.fiatRates.reloadData), Rx.incSec, 3 to 18 by 3)
      val fiatRepeat = Rx.repeat(fiatRetry, Rx.incSec, fiatPeriodSecs to Int.MaxValue by fiatPeriodSecs)
      val fiatObs = Rx.initDelay(fiatRepeat, LNParams.fiatRates.info.stamp, fiatPeriodSecs * 1000L)
      fiatObs.foreach(LNParams.fiatRates.updateInfo, none)
    }
  }

  def vulnerableChannelsExist: Boolean = LNParams.cm.allNormal.flatMap(Channel.chanAndCommitsOpt).exists {
    case ChanAndCommits(_, normalCommits: NormalCommits) => normalCommits.remoteNextHtlcId > 0
    case _ => false
  }

  def reScheduleWatchtower: Unit =
    DelayedNotification.schedule(app,
      DelayedNotification.WATCH_TOWER_TAG,
      title = app.getString(delayed_notify_wt_title),
      body = app.getString(delayed_notify_wt_body),
      DelayedNotification.WATCH_TOWER_PERIOD_MSEC)

  def reScheduleInFlight: Unit =
    DelayedNotification.schedule(app,
      DelayedNotification.IN_FLIGHT_HTLC_TAG,
      title = app.getString(delayed_notify_pending_payment_title),
      body = app.getString(delayed_notify_pending_payment_body),
      DelayedNotification.IN_FLIGHT_HTLC_PERIOD_MSEC)

  // Fiat conversion

  def currentRate(rates: Fiat2Btc, code: String): Try[Double] = Try(rates apply code)
  def msatInFiat(rates: Fiat2Btc, code: String)(msat: MilliSatoshi): Try[Double] = currentRate(rates, code).map(perBtc => msat.toLong * perBtc / BtcDenomination.factor)
  val currentMsatInFiatHuman: MilliSatoshi => String = msat => msatInFiatHuman(LNParams.fiatRates.info.rates, fiatCode, msat, immortan.utils.Denomination.formatFiat)

  def msatInFiatHuman(rates: Fiat2Btc, code: String, msat: MilliSatoshi, decimalFormat: DecimalFormat): String = {
    val fiatAmount: String = msatInFiat(rates, code)(msat).map(decimalFormat.format).getOrElse(default = "?")
    val formatted = LNParams.fiatRates.customFiatSymbols.get(code).map(symbol => s"$symbol$fiatAmount")
    formatted.getOrElse(s"$fiatAmount $code")
  }
}

object Vibrator {
  private val vibrator = WalletApp.app.getSystemService(Context.VIBRATOR_SERVICE).asInstanceOf[android.os.Vibrator]
  def vibrate: Unit = if (null != vibrator && vibrator.hasVibrator) vibrator.vibrate(VibrationEffect.createWaveform(Array(0L, 85, 200), -1))
}

class WalletApp extends Application { me =>
  WalletApp.app = me

  lazy val foregroundServiceIntent = new Intent(me, AwaitService.awaitServiceClass)
  lazy val prefs: SharedPreferences = getSharedPreferences("prefs", Context.MODE_PRIVATE)

  private[this] lazy val metrics = getResources.getDisplayMetrics
  lazy val scrWidth: Double = metrics.widthPixels.toDouble / metrics.densityDpi
  lazy val maxDialog: Double = metrics.densityDpi * 2.3

  import android.provider.Settings.System.{FONT_SCALE, getFloat}
  // Special handling for cases when user has chosen large font and screen size is constrained
  lazy val tooFewSpace: Boolean = getFloat(getContentResolver, FONT_SCALE, 1) > 1 && scrWidth < 2.4

  lazy val dateFormat: SimpleDateFormat = DateFormat.is24HourFormat(me) match {
    case false if tooFewSpace => new SimpleDateFormat("MM/dd/yy")
    case true if tooFewSpace => new SimpleDateFormat("dd/MM/yy")
    case false => new SimpleDateFormat("MMM dd, yyyy")
    case true => new SimpleDateFormat("d MMM yyyy")
  }

  lazy val plur: (Array[String], Long) => String = getString(R.string.lang) match {
    case "eng" | "esp" | "port" => (opts: Array[String], num: Long) => if (num == 1) opts(1) else opts(2)
    case "chn" | "jpn" => (phraseOptions: Array[String], _: Long) => phraseOptions(1)
    case "rus" | "ukr" => (phraseOptions: Array[String], num: Long) =>

      val reminder100 = num % 100
      val reminder10 = reminder100 % 10
      if (reminder100 > 10 & reminder100 < 20) phraseOptions(3)
      else if (reminder10 > 1 & reminder10 < 5) phraseOptions(2)
      else if (reminder10 == 1) phraseOptions(1)
      else phraseOptions(3)
  }

  override def attachBaseContext(base: Context): Unit = {
    super.attachBaseContext(base)
    MultiDex.install(me)
  }

  override def onCreate: Unit = runAnd(super.onCreate) {
    // Currently night theme is the only option, should be set by default
    AppCompatDelegate.setDefaultNightMode(AppCompatDelegate.MODE_NIGHT_YES)

    if (Build.VERSION.SDK_INT > Build.VERSION_CODES.N_MR1) {
      val manager = this getSystemService classOf[NotificationManager]
      val chan1 = new NotificationChannel(AwaitService.CHANNEL_ID, "Foreground notifications", NotificationManager.IMPORTANCE_LOW)
      val chan2 = new NotificationChannel(DelayedNotification.CHANNEL_ID, "Scheduled notifications", NotificationManager.IMPORTANCE_LOW)
      manager.createNotificationChannel(chan1)
      manager.createNotificationChannel(chan2)
    }

    ChannelMaster.inFinalized.foreach { _ =>
      // Delayed notification is removed when payment gets either failed or fulfilled
      if (LNParams.cm.inProcessors.isEmpty) stopService(foregroundServiceIntent)
    }

    Rx.uniqueFirstAndLastWithinWindow(ChannelMaster.stateUpdateStream, 500.millis).foreach { _ =>
      // This might be the last channel state update which clears all in-flight HTLCs
      DelayedNotification.cancel(me, DelayedNotification.IN_FLIGHT_HTLC_TAG)
      if (LNParams.cm.channelsContainHtlc) WalletApp.reScheduleInFlight
    }
  }

  override def onTrimMemory(level: Int): Unit = {
    val shouldResetUnlock = level == ComponentCallbacks2.TRIM_MEMORY_UI_HIDDEN
    if (shouldResetUnlock) AppLock.getInstance(me).setAuthenticationRequired
    super.onTrimMemory(level)
  }

  def when(thenDate: Date, simpleFormat: SimpleDateFormat, now: Long = System.currentTimeMillis): String = thenDate.getTime match {
    case tooLongAgo if now - tooLongAgo > 12960000 || tooFewSpace || WalletApp.denom == BtcDenomination => simpleFormat.format(thenDate)
    case ago => android.text.format.DateUtils.getRelativeTimeSpanString(ago, now, 0).toString
  }

  def showStickyNotification(titleRes: Int, bodyRes: Int, amount: MilliSatoshi): Unit = {
    val withTitle = foregroundServiceIntent.putExtra(AwaitService.TITLE_TO_DISPLAY, me getString titleRes)
    val bodyText = getString(bodyRes).format(WalletApp.denom.parsedWithSign(amount, Colors.cardIn, Colors.cardZero).html)
    val withBodyAction = withTitle.putExtra(AwaitService.BODY_TO_DISPLAY, bodyText).setAction(AwaitService.ACTION_SHOW)
    androidx.core.content.ContextCompat.startForegroundService(me, withBodyAction)
  }

  def quickToast(code: Int): Unit = quickToast(me getString code)
  def quickToast(msg: CharSequence): Unit = Toast.makeText(me, msg, Toast.LENGTH_LONG).show
  def plurOrZero(num: Long, opts: Array[String] = Array.empty): String = if (num > 0) plur(opts, num).format(num) else opts(0)
  def clipboardManager: ClipboardManager = getSystemService(Context.CLIPBOARD_SERVICE).asInstanceOf[ClipboardManager]
  def getBufferUnsafe: String = clipboardManager.getPrimaryClip.getItemAt(0).getText.toString

  def inputMethodManager: InputMethodManager = getSystemService(Context.INPUT_METHOD_SERVICE).asInstanceOf[InputMethodManager]
  def showKeys(field: EditText): Unit = try inputMethodManager.showSoftInput(field, InputMethodManager.SHOW_IMPLICIT) catch none
  def hideKeys(field: EditText): Unit = try inputMethodManager.hideSoftInputFromWindow(field.getWindowToken, 0) catch none

  def copy(text: String): Unit = {
    val bufferContent = ClipData.newPlainText("wallet", text)
    clipboardManager.setPrimaryClip(bufferContent)
    quickToast(copied_to_clipboard)
  }
}
