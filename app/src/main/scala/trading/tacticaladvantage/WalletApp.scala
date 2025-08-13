package trading.tacticaladvantage

import akka.actor.Props
import android.app.Application
import android.content._
import android.text.format.DateFormat
import android.view.inputmethod.InputMethodManager
import android.widget.{EditText, Toast}
import androidx.multidex.MultiDex
import fr.acinq.bitcoin.{Block, ByteVector32, Satoshi, SatoshiLong}
import fr.acinq.eclair._
import fr.acinq.eclair.blockchain.electrum.ElectrumWallet.{TransactionReceived, chainHash}
import fr.acinq.eclair.blockchain.electrum._
import immortan._
import immortan.crypto.Tools._
import immortan.sqlite._
import immortan.utils.ImplicitJsonFormats._
import immortan.utils._
import spray.json._
import trading.tacticaladvantage.R.string._
import trading.tacticaladvantage.sqlite._

import java.net.InetSocketAddress
import java.text.{DecimalFormat, SimpleDateFormat}
import java.util.Date
import scala.collection.mutable
import scala.util.Try

object WalletApp {
  var extDataBag: SQLiteData = _
  var btcTxDataBag: SQLiteBtcTx = _
  var btcWalletBag: SQLiteBtcWallet = _

  var usdtTxDataBag: SQLiteUsdtTx = _
  var usdtWalletBag: SQLiteUsdtWallet = _

  var feeRates: FeeRates = _
  var fiatRates: FiatRates = _
  var secret: WalletSecret = _
  var biconomy: Biconomy = _
  var taLink: TaLink = _
  var app: WalletApp = _

  val seenBtcInfos = mutable.Map.empty[ByteVector32, BtcInfo]
  val pendingBtcInfos = mutable.Map.empty[ByteVector32, BtcInfo]
  var currentBtcNode = Option.empty[InetSocketAddress]

  final val FIAT_CODE = "fiatCode"
  def fiatCode: String = app.prefs.getString(FIAT_CODE, "usd")
  def denom: Denomination = BtcDenomination

  def isAlive: Boolean =
    null != btcTxDataBag && null != btcWalletBag &&
      null != usdtTxDataBag && null != usdtWalletBag &&
      null != extDataBag && null != app

  def isOperational: Boolean =
    null != ElectrumWallet.chainHash && null != secret && null != fiatRates &&
      null != feeRates && null != ElectrumWallet.connectionProvider &&
      biconomy != null && taLink != null

  def freePossiblyUsedRuntimeResouces: Unit = {
    try ElectrumWallet.becomeShutDown catch none
    try fiatRates.becomeShutDown catch none
    try feeRates.becomeShutDown catch none
    try taLink.becomeShutDown catch none
    // non-alive and non-operational
    btcTxDataBag = null
    secret = null
  }

  def makeAlive: Unit = {
    ElectrumWallet.chainHash = Block.LivenetGenesisBlock.hash
    val interface = new DBInterfaceSQLiteAndroid(app, "misc.db")

    interface txWrap {
      extDataBag = new SQLiteData(interface)
      btcTxDataBag = new SQLiteBtcTx(interface)
      btcWalletBag = new SQLiteBtcWallet(interface)

      usdtTxDataBag = new SQLiteUsdtTx(interface)
      usdtWalletBag = new SQLiteUsdtWallet(interface)
    }
  }

  def makeOperational(sec: WalletSecret): Unit = {
    require(isAlive, "Halted, application is not alive yet")
    secret = sec

    ElectrumWallet.params = WalletParameters(extDataBag, btcWalletBag, btcTxDataBag, dustLimit = 546L.sat)
    ElectrumWallet.connectionProvider = new ClearnetConnectionProvider
    biconomy = new Biconomy(ElectrumWallet.connectionProvider)

    taLink = new TaLink("wss://localhost") {
      def loadData: Try[TaLink.TaData] = extDataBag.tryGet("ta-data").map(SQLiteData.byteVecToString) map to[TaLink.TaData]
      def saveData(data: TaLink.TaData): Unit = extDataBag.put("ta-data", data.toJson.compactPrint getBytes "UTF-8")
    }

    extDataBag.db txWrap {
      feeRates = new FeeRates(extDataBag)
      fiatRates = new FiatRates(extDataBag)
    }

    ElectrumClientPool.loadFromChainHash = {
      case Block.LivenetGenesisBlock.hash => ElectrumClientPool.readServerAddresses(app.getAssets open "servers_mainnet.json")
      case Block.TestnetGenesisBlock.hash => ElectrumClientPool.readServerAddresses(app.getAssets open "servers_testnet.json")
      case _ => throw new RuntimeException
    }

    CheckPoint.loadFromChainHash = {
      case Block.LivenetGenesisBlock.hash => CheckPoint.load(app.getAssets open "checkpoints_mainnet.json")
      case Block.TestnetGenesisBlock.hash => CheckPoint.load(app.getAssets open "checkpoints_testnet.json")
      case _ => throw new RuntimeException
    }

    ElectrumWallet.pool = ElectrumWallet.system.actorOf(Props(classOf[ElectrumClientPool], ElectrumWallet.chainHash, ElectrumWallet.ec), "pool")
    ElectrumWallet.sync = ElectrumWallet.system.actorOf(Props(classOf[ElectrumChainSync], ElectrumWallet.pool, ElectrumWallet.params.headerDb, ElectrumWallet.chainHash), "sync")
    ElectrumWallet.catcher = ElectrumWallet.system.actorOf(Props(new WalletEventsCatcher), "catcher")

    // Fill online wallet map with persisted wallets, it will be empty if we have none
    val (native, attached) = btcWalletBag.listWallets.partition(_.core.attachedMaster.isDefined)
    for (info \ ord <- native.zipWithIndex) startFromInfo(info, ord)
    for (info <- attached) startFromInfo(info, ord = 0L)

    if (ElectrumWallet.specs.isEmpty) {
      val label = app.getString(bitcoin_wallet)
      val core = SigningWallet(ElectrumWallet.BIP84)
      val ewt = ElectrumWalletType.makeSigningType(core.walletType, secret.keys.bitcoinMaster, chainHash, ord = 0L)
      val spec = ElectrumWallet.makeSigningWalletParts(core, ewt, lastBalance = Satoshi(0L), label)
      ElectrumWallet.addWallet(spec)
    }

    feeRates.listeners += new FeeRatesListener {
      def onFeeRates(info: FeeRatesInfo): Unit =
        extDataBag.putFeeRatesInfo(info)
    }

    fiatRates.listeners += new FiatRatesListener {
      def onFiatRates(info: FiatRatesInfo): Unit =
        extDataBag.putFiatRatesInfo(info)
    }

    ElectrumWallet.catcher ! new WalletEventsListener {
      override def onChainDisconnected: Unit = currentBtcNode = Option.empty[InetSocketAddress]
      override def onChainMasterSelected(event: InetSocketAddress): Unit = currentBtcNode = event.asSome

      override def onTransactionReceived(event: TransactionReceived): Unit = {
        def addChainTx(received: Satoshi, sent: Satoshi, fee: Satoshi, description: BtcDescription, isIncoming: Long): Unit = btcTxDataBag.db txWrap {
          btcTxDataBag.addTx(event.tx, event.depth, received, sent, fee, event.xPubs, description, isIncoming, fiatRates.info.rates, event.stamp)
          btcTxDataBag.addSearchableTransaction(description.queryText(event.tx.txid), event.tx.txid)
          if (event.depth == 1) Vibrator.vibrate
        }

        pendingBtcInfos.remove(event.tx.txid)
        seenBtcInfos.get(event.tx.txid) match {
          case Some(seen) => addChainTx(seen.receivedSat, seen.sentSat, seen.feeSat, seen.description, isIncoming = seen.incoming)
          case None if event.received > event.sent => addChainTx(event.received - event.sent, event.sent, Satoshi(0L), PlainBtcDescription(event.addresses), isIncoming = 1L)
          case None => addChainTx(event.received, event.sent - event.received, Satoshi(0L), PlainBtcDescription(event.addresses), isIncoming = 0L)
        }
      }
    }

    ElectrumWallet.connectionProvider doWhenReady {
      ElectrumWallet.pool ! ElectrumClientPool.InitConnect

      val feeratePeriodHours = 2
      val rateRetry = Rx.retry(Rx.ioQueue.map(_ => feeRates.reloadData), Rx.incSec, 3 to 18 by 3)
      val rateRepeat = Rx.repeat(rateRetry, Rx.incHour, feeratePeriodHours to Int.MaxValue by feeratePeriodHours)
      val feerateObs = Rx.initDelay(rateRepeat, feeRates.info.stamp, feeratePeriodHours * 3600 * 1000L)
      feerateObs.foreach(feeRates.updateInfo, none)

      val fiatPeriodSecs = 60 * 3
      val fiatRetry = Rx.retry(Rx.ioQueue.map(_ => fiatRates.reloadData), Rx.incSec, 3 to 18 by 3)
      val fiatRepeat = Rx.repeat(fiatRetry, Rx.incSec, fiatPeriodSecs to Int.MaxValue by fiatPeriodSecs)
      fiatRepeat.foreach(fiatRates.updateInfo, none)
    }
  }

  def startFromInfo(info: CompleteBtcWalletInfo, ord: Long): Unit = {
    val ext = info.core.attachedMaster.getOrElse(secret.keys.bitcoinMaster)
    val ewt = ElectrumWalletType.makeSigningType(info.core.walletType, ext, chainHash, ord)
    val spec = ElectrumWallet.makeSigningWalletParts(info.core, ewt, info.lastBalance, info.label)
    ElectrumWallet.specs.update(ewt.xPub, spec)
    spec.walletRef ! info.initData
  }

  // Fiat conversion

  def currentRate(rates: Fiat2Btc, code: String): Try[Double] = Try(rates apply code)
  def msatInFiat(rates: Fiat2Btc, code: String)(msat: MilliSatoshi): Try[Double] = currentRate(rates, code).map(perBtc => msat.toLong * perBtc / BtcDenomination.factor)
  val currentMsatInFiatHuman: MilliSatoshi => String = msat => msatInFiatHuman(fiatRates.info.rates, fiatCode, msat, immortan.utils.Denomination.formatFiat)

  def msatInFiatHuman(rates: Fiat2Btc, code: String, msat: MilliSatoshi, decimalFormat: DecimalFormat): String = {
    val fiatAmount: String = msatInFiat(rates, code)(msat).map(decimalFormat.format).getOrElse(default = "?")
    fiatRates.customFiatSymbols.get(code).map(sign => s"$sign$fiatAmount").getOrElse(s"$fiatAmount $code")
  }
}

class WalletApp extends Application { me =>
  WalletApp.app = me

  private[this] lazy val metrics = getResources.getDisplayMetrics
  lazy val prefs: SharedPreferences = getSharedPreferences("prefs", Context.MODE_PRIVATE)
  lazy val scrWidth: Double = metrics.widthPixels.toDouble / metrics.densityDpi
  lazy val maxDialog: Double = metrics.densityDpi * 2.3

  import android.provider.Settings.System.{FONT_SCALE, getFloat}
  // Special handling for cases when user has chosen large font and screen size is constrained
  lazy val tooFewSpace: Boolean = getFloat(getContentResolver, FONT_SCALE, 1) > 1.15 || scrWidth < 2.4

  lazy val dateFormat: SimpleDateFormat = DateFormat.is24HourFormat(me) match {
    case false if tooFewSpace => new SimpleDateFormat("MM/dd/yy")
    case true if tooFewSpace => new SimpleDateFormat("dd/MM/yy")
    case false => new SimpleDateFormat("MMM dd, yyyy")
    case true => new SimpleDateFormat("d MMM yyyy")
  }

  override def attachBaseContext(base: Context): Unit = {
    super.attachBaseContext(base)
    MultiDex.install(me)
  }

  def when(thenDate: Date, simpleFormat: SimpleDateFormat): String =
    System.currentTimeMillis - thenDate.getTime match {
      case ago if ago > 12960000 => simpleFormat.format(thenDate)
      case ago if ago < android.text.format.DateUtils.MINUTE_IN_MILLIS => "now"
      case ago if ago < android.text.format.DateUtils.HOUR_IN_MILLIS => s"${ago / android.text.format.DateUtils.MINUTE_IN_MILLIS} min ago"
      case ago if ago < android.text.format.DateUtils.DAY_IN_MILLIS => s"${ago / android.text.format.DateUtils.HOUR_IN_MILLIS} hr ago"
    }

  def quickToast(code: Int): Unit = quickToast(me getString code)
  def quickToast(msg: CharSequence): Unit = Toast.makeText(me, msg, Toast.LENGTH_LONG).show
  def clipboardManager: ClipboardManager = getSystemService(Context.CLIPBOARD_SERVICE).asInstanceOf[ClipboardManager]
  def inputMethodManager: InputMethodManager = getSystemService(Context.INPUT_METHOD_SERVICE).asInstanceOf[InputMethodManager]
  def showKeys(field: EditText): Unit = try inputMethodManager.showSoftInput(field, InputMethodManager.SHOW_IMPLICIT) catch none
  def hideKeys(field: EditText): Unit = try inputMethodManager.hideSoftInputFromWindow(field.getWindowToken, 0) catch none

  def copy(text: String): Unit = {
    val bufferContent = ClipData.newPlainText("wallet", text)
    clipboardManager.setPrimaryClip(bufferContent)
    quickToast(copied_to_clipboard)
  }
}

object Vibrator {
  private val vibrator = WalletApp.app.getSystemService(Context.VIBRATOR_SERVICE).asInstanceOf[android.os.Vibrator]
  def vibrate: Unit = if (null != vibrator && vibrator.hasVibrator) vibrator.vibrate(Array(0L, 85, 200), -1)
}