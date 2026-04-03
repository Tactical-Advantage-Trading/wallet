package trading.tacticaladvantage

import akka.actor.{PoisonPill, Props}
import android.app.Application
import android.content._
import android.icu.text.RelativeDateTimeFormatter.{Direction, RelativeUnit, Style}
import android.icu.text.{DisplayContext, NumberFormat, RelativeDateTimeFormatter}
import android.icu.util.ULocale
import android.text.format.{DateFormat, DateUtils}
import android.view.inputmethod.InputMethodManager
import android.widget.{EditText, Toast}
import androidx.multidex.MultiDex
import fr.acinq.bitcoin.DeterministicWallet.{ExtendedPrivateKey, ExtendedPublicKey}
import fr.acinq.bitcoin.{Block, Satoshi}
import fr.acinq.eclair._
import fr.acinq.eclair.blockchain.electrum.ElectrumWallet.TransactionReceived
import fr.acinq.eclair.blockchain.electrum._
import immortan.Tools._
import immortan._
import immortan.sqlite._
import immortan.utils._
import trading.tacticaladvantage.R.string._
import trading.tacticaladvantage.sqlite._
import trading.tacticaladvantage.utils.WsListener

import java.net.InetSocketAddress
import java.text.{DecimalFormat, SimpleDateFormat}
import java.util.{Date, Locale}
import scala.collection.mutable
import scala.util.Try


class NetworkWalletGroup(val id: Int, ticker: String, genesis: Block) {
  val connectionProvider: ConnectionProvider = new ClearnetConnectionProvider
  val pendingInfos = mutable.Map.empty[String, ItemDetails]
  val seenInfos = mutable.Map.empty[String, ItemDetails]
  var currentNode = Option.empty[InetSocketAddress]

  var walletBag: SQLiteWallet = _
  var extDataBag: SQLiteData = _
  var txDataBag: SQLiteTx = _

  var master: ExtendedPrivateKey = _
  var fiatRates: FiatRates = _
  var feeRates: FeeRates = _
  var electrum: Electrum = _

  def isAlive: Boolean = null != txDataBag && null != walletBag && null != extDataBag
  def isOperational: Boolean = null != master && null != electrum && null != fiatRates && null != feeRates

  def freePossiblyUsedRuntimeResouces: Unit = {
    try fiatRates.becomeShutDown catch none
    try feeRates.becomeShutDown catch none
    try electrum.becomeShutDown catch none
    // non-alive and non-operational
    txDataBag = null
    master = null
  }

  def makeAlive(app: WalletApp): Unit = {
    val interface = new DBInterfaceSQLiteAndroid(app, s"$id.db")

    interface txWrap {
      walletBag = new SQLiteWallet(interface)
      extDataBag = new SQLiteData(interface)
      txDataBag = new SQLiteTx(interface)
    }
  }

  def makeOperational(app: WalletApp, key: ExtendedPrivateKey, servers: String, checkpoints: String, strict: Boolean): Unit = {
    electrum = new Electrum(WalletParameters(extDataBag, walletBag, txDataBag), genesis.hash)
    master = key

    extDataBag.db txWrap {
      feeRates = new FeeRates(extDataBag)
      fiatRates = new FiatRates(extDataBag)
    }

    feeRates.listeners += new FeeRatesListener {
      def onFeeRates(info: FeeRatesInfo): Unit =
        extDataBag.putFeeRatesInfo(info)
    }

    fiatRates.listeners += new FiatRatesListener {
      def onFiatRates(info: FiatRatesInfo): Unit =
        extDataBag.putFiatRatesInfo(info)
    }

    electrum.pool = electrum.system.actorOf(Props(classOf[ElectrumClientPool], app.getAssets open servers), "pool")
    electrum.sync = electrum.system.actorOf(Props(classOf[ElectrumChainSync], electrum, app.getAssets open checkpoints, strict), "sync")
    electrum.catcher = electrum.system.actorOf(Props(new WalletEventsCatcher), "catcher")

    electrum.catcher ! new WalletEventsListener {
      override def onTransactionReceived(event: TransactionReceived): Unit = {
        def addTx(received: Satoshi, sent: Satoshi, fee: Satoshi, description: CoinDescription, isIncoming: Long): Unit = txDataBag.db txWrap {
          txDataBag.addTx(event.tx, event.depth, received, sent, fee, event.xPubs, description, isIncoming, fiatRates.info.rates, event.stamp)
          txDataBag.addSearchableTransaction(description.queryText(event.tx.txid), event.tx.txid)
          pendingInfos.remove(event.tx.txid.toHex)
        }

        seenInfos.get(event.tx.txid.toHex) match {
          case Some(seen: CoinDetails) => addTx(seen.receivedSat, seen.sentSat, seen.feeSat, seen.description, isIncoming = seen.incoming)
          case None if event.received > event.sent => addTx(event.received - event.sent, event.sent, Satoshi(0L), CoinDescription(event.addresses, None, id), isIncoming = 1L)
          case None => addTx(event.received, event.sent - event.received, Satoshi(0L), CoinDescription(event.addresses, label = None, id), isIncoming = 0L)
        }
      }

      override def onChainDisconnected: Unit = currentNode = Option.empty[InetSocketAddress]
      override def onChainMasterSelected(event: InetSocketAddress): Unit = currentNode = event.asSome
    }
  }

  def createBtcWallet(ord: Long = 0L): WalletSpec = {
    val ewt = ElectrumWalletType.makeSigningType(ElectrumWallet.BIP84, master, electrum.chainHash, ord)
    val btcSpec = electrum.makeSigningWalletParts(SigningWallet(ElectrumWallet.BIP84), ewt, lastBalance = Satoshi(0L), ticker)
    walletBag.addWallet(btcSpec.info, electrum.params.emptyPersistentDataBytes, btcSpec.data.keys.ewt.xPub.publicKey)
    btcSpec
  }

  def attachWallet(master1: ExtendedPrivateKey): Unit = {
    val core = SigningWallet(ElectrumWallet.BIP84, attachedMaster = master1.asSome)
    val ewt = ElectrumWalletType.makeSigningType(core.walletType, master1, electrum.chainHash, 0L)
    val spec = electrum.makeSigningWalletParts(core, ewt, lastBalance = Satoshi(0L), label = ticker)
    walletBag.addWallet(spec.info, electrum.params.emptyPersistentDataBytes, spec.data.keys.ewt.xPub.publicKey)
    postInitWallet(spec)
  }

  def initWallet(info: CompleteWalletInfo, ord: Long): Unit = {
    val ext = info.core.attachedMaster.getOrElse(WalletApp.secret.keys.bitcoinMaster)
    val ewt = ElectrumWalletType.makeSigningType(info.core.walletType, ext, electrum.chainHash, ord)
    val spec = electrum.makeSigningWalletParts(info.core, ewt, info.lastBalance, info.label)
    electrum.specs.update(ewt.xPub, spec)
    spec.walletRef ! info.initData
  }

  def postInitWallet(spec: WalletSpec): Unit = {
    electrum.specs.update(spec.data.keys.ewt.xPub, spec)
    spec.walletRef ! electrum.params.emptyPersistentDataBytes
    electrum.sync ! ElectrumWallet.ChainFor(spec.walletRef)
  }

  def removeWallet(key: ExtendedPublicKey): Unit = {
    electrum.specs.remove(key).foreach(_.walletRef ! PoisonPill)
    walletBag.remove(key.publicKey)
  }

  def initWallets = {
    val (native, attached) = walletBag.listWallets.partition(_.core.attachedMaster.isDefined)
    for (walletInfo \ ord <- native.zipWithIndex) initWallet(walletInfo, ord)
    for (walletInfo <- attached) initWallet(walletInfo, ord = 0L)

    connectionProvider doWhenReady {
      electrum.pool ! ElectrumClientPool.InitConnect

      val feeratePeriodHours = 2
      val rateRetry = Rx.retry(Rx.ioQueue.map(_ => feeRates reloadData connectionProvider), Rx.incSec, 3 to 18 by 3)
      val rateRepeat = Rx.repeat(rateRetry, Rx.incHour, feeratePeriodHours to Int.MaxValue by feeratePeriodHours)
      val feerateObs = Rx.initDelay(rateRepeat, feeRates.info.stamp, feeratePeriodHours * 3600 * 1000L)
      feerateObs.foreach(feeRates.updateInfo, none)

      val fiatPeriodSecs = 60 * 3
      val fiatRetry = Rx.retry(Rx.ioQueue.map(_ => fiatRates reloadData connectionProvider), Rx.incSec, 3 to 18 by 3)
      val fiatRepeat = Rx.repeat(fiatRetry, Rx.incSec, fiatPeriodSecs to Int.MaxValue by fiatPeriodSecs)
      fiatRepeat.foreach(fiatRates.updateInfo, none)
    }
  }
}

object WalletApp {
  final val ID_BTC = 1
  final val ID_ECA = 2
  final val FIAT_CODE = "fiatCode"
  final val SHOW_TA_CARD = "showTaCard"
  val btc = new NetworkWalletGroup(WalletApp.ID_BTC, "BTC", Block.LivenetGenesisBlock)
  val eca = new NetworkWalletGroup(WalletApp.ID_ECA, "ECA", Block.TestnetGenesisBlock)

  var linkClient: LinkClient = _
  var secret: WalletSecret = _
  var app: WalletApp = _

  def fiatCode: String = app.prefs.getString(FIAT_CODE, "usd")
  def getShowTaCard: Boolean = app.prefs.getBoolean(SHOW_TA_CARD, true)
  def setShowTaCard(show: Boolean) = app.prefs.edit.putBoolean(SHOW_TA_CARD, show).commit

  def isAlive: Boolean = null != app && null != btc && null != eca && btc.isAlive && eca.isAlive
  def isOperational: Boolean = null != secret && null != linkClient && btc.isOperational && eca.isOperational

  def freePossiblyUsedRuntimeResouces = {
    try btc.freePossiblyUsedRuntimeResouces catch none
    try eca.freePossiblyUsedRuntimeResouces catch none
    secret = null
  }

  def makeAlive = {
    btc.makeAlive(app)
    eca.makeAlive(app)
  }

  def makeOperational(sec: WalletSecret) = {
    btc.makeOperational(app, sec.keys.bitcoinMaster, "btc_servers.json", "btc_checkpoints.json", strict = true)
    eca.makeOperational(app, sec.keys.ecashMaster, "eca_servers.json", "eca_checkpoints.json", strict = false)
    secret = sec

    linkClient = new LinkClient(btc.extDataBag)
    linkClient ! new LinkClient.Listener(LinkClient.USER_UPDATE) {
      override def onConnected(stateData: LinkClient.TaLinkState): Unit = stateData match {
        case data: LinkClient.UserStatus => linkClient ! LinkClient.Request(LinkClient.GetUserStatus(data.sessionToken), id)
        case _ => // Not logged in, user can do it manually later
      }

      override def onResponse(arguments: Option[LinkClient.ResponseArguments] = None): Unit = arguments.foreach {
        case LinkClient.Failure(LinkClient.NOT_AUTHORIZED) => linkClient ! LinkClient.LoggedOut
        case status: LinkClient.UserStatus => linkClient ! status
        case _ =>
      }
    }
  }

  def initTaCard = if (getShowTaCard) {
    // This is a single place where we should bypass sequential threading
    for (status <- linkClient.loadUserStatus) linkClient.data = status
    linkClient ! WsListener.CmdConnect
  }

  def initWallets = {
    btc.initWallets
    eca.initWallets
    initTaCard
  }

  // Fiat conversion

  def currentRate(rates: Fiat2Btc, code: String): Try[Double] = Try(rates apply code)
  def msatInFiat(rates: Fiat2Btc, code: String)(msat: MilliSatoshi): Try[Double] = currentRate(rates, code).map(perBtc => msat.toLong * perBtc / BtcDenom.factor)
  def currentMsatInFiatHuman(fr: FiatRates, msat: MilliSatoshi): String = msatInFiatHuman(fr, fiatCode, msat, Denomination.formatFiatShort)

  def msatInFiatHuman(fr: FiatRates, code: String, msat: MilliSatoshi, decimalFormat: DecimalFormat): String = {
    val fiatAmount: String = msatInFiat(fr.info.rates, code)(msat).map(decimalFormat.format).getOrElse(default = "?")
    fr.customFiatSymbols.get(code).map(sign => s"$sign$fiatAmount").getOrElse(s"$fiatAmount $code")
  }

  val uLocale = ULocale.forLocale(Locale.getDefault)
  val rfmt = RelativeDateTimeFormatter.getInstance(uLocale,
    NumberFormat.getInstance(uLocale), Style.NARROW,
    DisplayContext.CAPITALIZATION_NONE)

  def when(thenDate: Date, f: SimpleDateFormat): String = {
    val deltaMs = thenDate.getTime - System.currentTimeMillis
    val dir = if (deltaMs >= 0) Direction.NEXT else Direction.LAST

    math.abs(deltaMs) match {
      case absMs if absMs < DateUtils.MINUTE_IN_MILLIS => "now"
      case absMs if absMs < DateUtils.HOUR_IN_MILLIS =>
        val mins = math.round(absMs / DateUtils.MINUTE_IN_MILLIS.toDouble)
        rfmt.format(mins.toDouble, dir, RelativeUnit.MINUTES)
      case absMs if absMs < DateUtils.DAY_IN_MILLIS =>
        val hours = math.round(absMs / DateUtils.HOUR_IN_MILLIS.toDouble)
        rfmt.format(hours.toDouble, dir, RelativeUnit.HOURS)
      case absMs if absMs < DateUtils.WEEK_IN_MILLIS =>
        val days = math.round(absMs / DateUtils.DAY_IN_MILLIS.toDouble)
        rfmt.format(days.toDouble, dir, RelativeUnit.DAYS)
      case _ => f.format(thenDate)
    }
  }
}

class WalletApp extends Application { me =>
  WalletApp.app = me

  private[this] lazy val metrics = getResources.getDisplayMetrics
  lazy val prefs: SharedPreferences = getSharedPreferences("prefs", Context.MODE_PRIVATE)
  lazy val scrWidth: Double = metrics.widthPixels.toDouble / metrics.densityDpi
  lazy val maxDialog: Double = metrics.densityDpi * 2.3

  lazy val dateFormat: SimpleDateFormat = {
    val is24HourFormat = DateFormat.is24HourFormat(me)
    if (is24HourFormat) new SimpleDateFormat("dd/MM/yy")
    else new SimpleDateFormat("MM/dd/yy")
  }

  override def attachBaseContext(base: Context): Unit = {
    super.attachBaseContext(base)
    MultiDex.install(me)
  }

  def quickToast(code: Int): Unit = quickToast(me getString code)
  def quickToast(msg: CharSequence): Unit = Toast.makeText(me, msg, Toast.LENGTH_LONG).show
  def clipboardManager: ClipboardManager = getSystemService(Context.CLIPBOARD_SERVICE).asInstanceOf[ClipboardManager]
  def inputMethodManager: InputMethodManager = getSystemService(Context.INPUT_METHOD_SERVICE).asInstanceOf[InputMethodManager]
  def hideKeys(field: EditText): Unit = try inputMethodManager.hideSoftInputFromWindow(field.getWindowToken, 0) catch none

  def copy(text: String): Unit = {
    val bufferContent = ClipData.newPlainText("wallet", text)
    clipboardManager.setPrimaryClip(bufferContent)
    quickToast(copied_to_clipboard)
  }

  // Plurals

  lazy val plur = getString(lang) match {
    case "eng" | "esp" => (opts: Array[String], num: Int) => if (num == 1) opts(1) else opts(2)
    case "chn" | "jpn" => (phraseOptions: Array[String], num: Int) => phraseOptions(1)
    case "ukr" => (phraseOptions: Array[String], num: Int) =>
      val reminder100 = num % 100
      val reminder10 = reminder100 % 10
      if (reminder100 > 10 & reminder100 < 20) phraseOptions(3)
      else if (reminder10 > 1 & reminder10 < 5) phraseOptions(2)
      else if (reminder10 == 1) phraseOptions(1)
      else phraseOptions(3)
  }

  def plurOrZero(opts: Array[String], number: Int) =
    if (number > 0) plur(opts, number).format(number)
    else opts(0)
}