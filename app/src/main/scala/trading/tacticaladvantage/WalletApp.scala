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
import immortan.utils._
import trading.tacticaladvantage.R.string._
import trading.tacticaladvantage.sqlite._

import java.io.{File, FileOutputStream}
import java.net.InetSocketAddress
import java.text.{DecimalFormat, SimpleDateFormat}
import java.util.Date
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

  var seenBtcInfos = Map.empty[ByteVector32, BtcInfo]
  var pendingBtcInfos = Map.empty[ByteVector32, BtcInfo]
  var currentBtcNode = Option.empty[InetSocketAddress]

  var seenUsdtInfos = Map.empty[String, UsdtInfo]
  var pendingUsdtInfos = Map.empty[String, UsdtInfo]

  final val FIAT_CODE = "fiatCode"
  final val SHOW_TA_CARD = "showTaCard"
  def fiatCode: String = app.prefs.getString(FIAT_CODE, "usd")
  def showTaCard: Boolean = app.prefs.getBoolean(SHOW_TA_CARD, true)
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
      usdtTxDataBag = new SQLiteUsdtTx(interface)
      usdtWalletBag = new SQLiteUsdtWallet(interface)

      btcTxDataBag = new SQLiteBtcTx(interface)
      btcWalletBag = new SQLiteBtcWallet(interface)
    }
  }

  def makeOperational(sec: WalletSecret): Unit = {
    require(isAlive, "Halted, application is not alive yet")
    secret = sec

    ElectrumWallet.connectionProvider = new ClearnetConnectionProvider
    ElectrumWallet.params = WalletParameters(extDataBag, btcWalletBag, btcTxDataBag, dustLimit = 546L.sat)
    biconomy = new Biconomy(ElectrumWallet.connectionProvider, app.getFilesDir.getAbsolutePath)
    taLink = new TaLink("wss://localhost", usdtWalletBag, extDataBag, biconomy)

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

    // BTC

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

    ElectrumWallet.catcher ! new WalletEventsListener {
      override def onChainDisconnected: Unit = currentBtcNode = Option.empty[InetSocketAddress]
      override def onChainMasterSelected(event: InetSocketAddress): Unit = currentBtcNode = event.asSome

      override def onTransactionReceived(event: TransactionReceived): Unit = {
        def addTx(received: Satoshi, sent: Satoshi, fee: Satoshi, description: BtcDescription, isIncoming: Long): Unit = btcTxDataBag.db txWrap {
          btcTxDataBag.addTx(event.tx, event.depth, received, sent, fee, event.xPubs, description, isIncoming, fiatRates.info.rates, event.stamp)
          btcTxDataBag.addSearchableTransaction(description.queryText(event.tx.txid), event.tx.txid)
          pendingBtcInfos -= event.tx.txid
        }

        seenBtcInfos.get(event.tx.txid) match {
          case Some(seen) => addTx(seen.receivedSat, seen.sentSat, seen.feeSat, seen.description, isIncoming = seen.incoming)
          case None if event.received > event.sent => addTx(event.received - event.sent, event.sent, Satoshi(0L), PlainBtcDescription(event.addresses), isIncoming = 1L)
          case None => addTx(event.received, event.sent - event.received, Satoshi(0L), PlainBtcDescription(event.addresses), isIncoming = 0L)
        }
      }
    }

    // USDT

    taLink ! new TaLink.Listener(TaLink.USDT_STATE_UPDATE) {
      def doAddTx(tr: TaLink.UsdTransfer, desc: UsdtDescription, received: String, sent: String, fee: String, isIncoming: Long) = {
        usdtTxDataBag.addTx(UsdtDescription.POLYGON, tr.hash, tr.block, tr.isRemoved, received, sent, fee, desc, isIncoming, taLink.usdt.totalBalance.toString, tr.stamp)
        usdtTxDataBag.addSearchableTransaction(desc.queryText(tr.hash), tr.hash)
        pendingUsdtInfos -= tr.fromAddr
        seenUsdtInfos -= tr.fromAddr
      }

      def addTx(tr: TaLink.UsdTransfer) = seenUsdtInfos.get(tr.fromAddr) match {
        case Some(seen) => doAddTx(tr, seen.description, received = "0", tr.amount, seen.feeUsdtString, isIncoming = 0)
        case None if taLink.usdt.myAddresses.contains(tr.fromAddr) => doAddTx(tr, UsdtDescription(tr.fromAddr, tr.toAddr), received = "0", tr.amount, fee = "0", isIncoming = 0)
        case None => doAddTx(tr, desc = UsdtDescription(tr.fromAddr, tr.toAddr), received = tr.amount, sent = "0", fee = "0", isIncoming = 1)
      }

      override def onResponse(arguments: Option[TaLink.ResponseArguments] = None): Unit = arguments.foreach {
        case usdtUpdate: TaLink.UsdTransfers => usdtTxDataBag.db.txWrap(usdtUpdate.transfers foreach addTx)
        case usdtUpdate: TaLink.UsdBalanceNonce => taLink ! usdtUpdate
        case _ => // Not interested in anything else
      }

      override def onConnected(stateData: TaLink.TaLinkState): Unit = if (taLink.usdt.withRealAddress.nonEmpty) {
        val sub = TaLink.UsdSubscribe(taLink.usdt.myAddresses.toList, taLink.usdt.withRealAddress.head.chainTip)
        taLink ! TaLink.Request(sub, id)
      }
    }

    taLink ! new TaLink.Listener(TaLink.USER_STATUS_UPDATE) {
      override def onConnected(stateData: TaLink.TaLinkState): Unit = stateData match {
        case data: TaLink.UserStatus => taLink ! TaLink.Request(TaLink.GetUserStatus(data.sessionToken), id)
        case _ => // Not logged in, user can do it manually later
      }

      override def onResponse(arguments: Option[TaLink.ResponseArguments] = None): Unit = arguments.foreach {
        case TaLink.Failure(TaLink.NOT_AUTHORIZED) => taLink ! TaLink.LoggedOut
        case data: TaLink.UserStatus => taLink ! data
        case _ =>
      }
    }
  }

  def assetToInternal(assetName: String, destName: String): Unit = {
    val destFile = new File(app.getFilesDir, destName)
    val outStream = new FileOutputStream(destFile)
    val inStream = app.getAssets.open(assetName)

    try {
      val buffer = new Bytes(2048)
      var bytesRead = inStream.read(buffer)
      destFile.getParentFile.mkdirs

      while (bytesRead != -1) {
        outStream.write(buffer, 0, bytesRead)
        bytesRead = inStream.read(buffer)
      }
    } finally {
      inStream.close
      outStream.close
    }
  }

  def createBtcWallet(ord: Long) = {
    val btcLabel = app.getString(bitcoin_wallet)
    val core = SigningWallet(ElectrumWallet.BIP84)
    val ewt = ElectrumWalletType.makeSigningType(core.walletType, secret.keys.bitcoinMaster, ElectrumWallet.chainHash, ord)
    val spec = ElectrumWallet.makeSigningWalletParts(core, ewt, lastBalance = Satoshi(0L), btcLabel)
    ElectrumWallet.addWallet(spec)
  }

  def createUsdtWallet(ord: Long) = {
    val usdtLabel = app.getString(usdt_wallet)
    val xPriv = ElectrumWalletType.xPriv32(secret.keys.tokenMaster, ElectrumWallet.chainHash, ord).xPriv
    taLink ! CompleteUsdtWalletInfo(CompleteUsdtWalletInfo.NOADDRESS, xPriv.privateKey.toAccount, usdtLabel)
    taLink ! TaLink.CmdEnsureUsdtAccounts
  }

  def initBtcWallet(info: CompleteBtcWalletInfo, ord: Long): Unit = {
    val ext = info.core.attachedMaster.getOrElse(secret.keys.bitcoinMaster)
    val ewt = ElectrumWalletType.makeSigningType(info.core.walletType, ext, chainHash, ord)
    val spec = ElectrumWallet.makeSigningWalletParts(info.core, ewt, info.lastBalance, info.label)
    ElectrumWallet.specs.update(ewt.xPub, spec)
    spec.walletRef ! info.initData
  }

  def init = {
    // Fill online map with persisted wallets
    val (native, attached) = btcWalletBag.listWallets.partition(_.core.attachedMaster.isDefined)
    for (btcWalletInfo \ ord <- native.zipWithIndex) initBtcWallet(btcWalletInfo, ord)
    for (btcWalletInfo <- attached) initBtcWallet(btcWalletInfo, ord = 0L)

    if (taLink.usdt.wallets.nonEmpty || showTaCard) {
      for (status <- taLink.loadUserStatus) taLink.data = status
      taLink ! TaLink.CmdEnsureUsdtAccounts
      taLink ! TaLink.CmdConnect
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