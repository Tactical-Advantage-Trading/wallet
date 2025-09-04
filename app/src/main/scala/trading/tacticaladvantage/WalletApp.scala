package trading.tacticaladvantage

import akka.actor.{PoisonPill, Props}
import android.app.Application
import android.content._
import android.text.format.DateFormat
import android.widget.Toast
import androidx.multidex.MultiDex
import fr.acinq.bitcoin.DeterministicWallet.ExtendedPublicKey
import fr.acinq.bitcoin.{Block, Satoshi}
import fr.acinq.eclair._
import fr.acinq.eclair.blockchain.electrum.ElectrumWallet.{TransactionReceived, chainHash}
import fr.acinq.eclair.blockchain.electrum._
import immortan.Tools._
import immortan._
import immortan.sqlite._
import immortan.utils._
import trading.tacticaladvantage.R.string._
import trading.tacticaladvantage.sqlite._
import trading.tacticaladvantage.utils.WsListener

import java.io.{File, FileOutputStream}
import java.net.InetSocketAddress
import java.text.{DecimalFormat, SimpleDateFormat}
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
  var linkClient: LinkClient = _
  var linkUsdt: LinkUsdt = _
  var biconomy: Biconomy = _
  var app: WalletApp = _

  var currentBtcNode = Option.empty[InetSocketAddress]
  val seenInfos = mutable.Map.empty[String, ItemDetails]
  val pendingInfos = mutable.Map.empty[String, ItemDetails]

  final val FIAT_CODE = "fiatCode"
  final val SHOW_TA_CARD = "showTaCard"
  def fiatCode: String = app.prefs.getString(FIAT_CODE, "usd")
  def showTaCard: Boolean = app.prefs.getBoolean(SHOW_TA_CARD, true)

  def setTaCard(visible: Boolean) = {
    app.prefs.edit.putBoolean(SHOW_TA_CARD, visible).commit
    DbStreams.next(DbStreams.walletStream)
  }

  def isAlive: Boolean =
    null != btcTxDataBag && null != btcWalletBag && null != usdtTxDataBag &&
      null != usdtWalletBag && null != extDataBag && null != app

  def isOperational: Boolean =
    null != ElectrumWallet.chainHash && null != secret && null != fiatRates &&
      null != feeRates && linkClient != null && linkUsdt != null && biconomy != null

  def freePossiblyUsedRuntimeResouces: Unit = {
    try ElectrumWallet.becomeShutDown catch none

    try fiatRates.becomeShutDown catch none
    try feeRates.becomeShutDown catch none

    try linkClient.becomeShutDown catch none
    try linkUsdt.becomeShutDown catch none
    // non-alive and non-operational
    btcTxDataBag = null
    secret = null
  }

  def makeAlive: Unit = {
    ElectrumWallet.chainHash = Block.TestnetGenesisBlock.hash
    val interface = new DBInterfaceSQLiteAndroid(app, "misc.db")

    interface txWrap {
      extDataBag = new SQLiteData(interface)
      usdtTxDataBag = new SQLiteUsdtTx(interface)
      usdtWalletBag = new SQLiteUsdtWallet(interface)

      btcTxDataBag = new SQLiteBtcTx(interface)
      btcWalletBag = new SQLiteBtcWallet(interface)
    }

    ElectrumWallet.connectionProvider = new ClearnetConnectionProvider
    ElectrumWallet.params = WalletParameters(extDataBag, btcWalletBag, btcTxDataBag)
  }

  def makeOperational(sec: WalletSecret): Unit = {
    require(isAlive, "Halted, application must be made alive before making it operational")
    biconomy = new Biconomy(ElectrumWallet.connectionProvider, app.getFilesDir.getAbsolutePath)
    linkUsdt = new LinkUsdt(usdtWalletBag, biconomy)
    linkClient = new LinkClient(extDataBag)
    secret = sec

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
          pendingInfos.remove(event.tx.txid.toHex)
        }

        seenInfos.get(event.tx.txid.toHex) match {
          case Some(seen: BtcInfo) => addTx(seen.receivedSat, seen.sentSat, seen.feeSat, seen.description, isIncoming = seen.incoming)
          case None if event.received > event.sent => addTx(event.received - event.sent, event.sent, Satoshi(0L), PlainBtcDescription(event.addresses), isIncoming = 1L)
          case None => addTx(event.received, event.sent - event.received, Satoshi(0L), PlainBtcDescription(event.addresses), isIncoming = 0L)
        }
      }
    }

    // USDT and Client

    linkUsdt ! new LinkUsdt.Listener(LinkUsdt.USDT_UPDATE) {
      def doAddTx(tr: LinkUsdt.UsdtTransfer, desc: UsdtDescription, received: String, sent: String, fee: String, isIncoming: Long) = {
        usdtTxDataBag.addTx(UsdtDescription.POLYGON, tr.hash, tr.block, tr.isRemoved, received, sent, fee, desc, isIncoming, linkUsdt.data.totalBalance.toString, tr.stamp)
        usdtTxDataBag.addSearchableTransaction(desc.queryText(tr.hash), tr.hash)
        pendingInfos.remove(tr.fromAddr)
      }

      def addTx(tr: LinkUsdt.UsdtTransfer) = pendingInfos.get(tr.fromAddr) match {
        case Some(seen: UsdtInfo) => doAddTx(tr, seen.description, received = "0", tr.amount, seen.feeUsdtString, isIncoming = seen.incoming)
        case None if linkUsdt.data.okWallets.contains(tr.fromAddr) => doAddTx(tr, tr.desc, received = "0", sent = tr.amount, fee = "0", isIncoming = 0)
        case None if linkUsdt.data.okWallets.contains(tr.toAddr) => doAddTx(tr, tr.desc, received = tr.amount, sent = "0", fee = "0", isIncoming = 1)
        case _ => // Unrelated to our current wallet state
      }

      override def onConnected: Unit = {
        for (info <- linkUsdt.data.withRealAddress) {
          val sub = LinkUsdt.UsdtSubscribe(info.lcAddress, info.chainTip)
          linkUsdt ! LinkUsdt.Request(sub, id)
        }
      }

      override def onResponse(arguments: Option[LinkUsdt.ResponseArguments] = None): Unit = arguments.foreach {
        case usdtUpdate: LinkUsdt.UsdtTransfers => usdtTxDataBag.db.txWrap { usdtUpdate.transfers foreach addTx }
        case usdtUpdate: LinkUsdt.UsdtBalanceNonce => linkUsdt ! usdtUpdate
        case _ => // Not interested in anything else
      }
    }

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

  def createUsdtWallet(sec: WalletSecret, ord: Long = 0L): CompleteUsdtWalletInfo = {
    val xPriv = ElectrumWalletType.xPriv32(master = sec.keys.tokenMaster, ElectrumWallet.chainHash, ord).xPriv
    val usdtInfo = CompleteUsdtWalletInfo(CompleteUsdtWalletInfo.NOADDRESS, xPriv.privateKey.toAccount, app.getString(usdt_wallet).trim)
    usdtWalletBag.addUpdateWallet(usdtInfo)
    DbStreams.next(DbStreams.walletStream)
    usdtInfo
  }

  def createBtcWallet(sec: WalletSecret, ord: Long = 0L): WalletSpec = {
    val ewt = ElectrumWalletType.makeSigningType(ElectrumWallet.BIP84, sec.keys.bitcoinMaster, ElectrumWallet.chainHash, ord)
    val btcSpec = ElectrumWallet.makeSigningWalletParts(SigningWallet(ElectrumWallet.BIP84), ewt, Satoshi(0L), app getString bitcoin_wallet)
    btcWalletBag.addWallet(btcSpec.info, ElectrumWallet.params.emptyPersistentDataBytes, btcSpec.data.keys.ewt.xPub.publicKey)
    btcSpec
  }

  def attachBtcWallet(keys: MasterKeys): Unit = {
    val core = SigningWallet(ElectrumWallet.BIP84, attachedMaster = keys.bitcoinMaster.asSome)
    val ewt = ElectrumWalletType.makeSigningType(core.walletType, keys.bitcoinMaster, ElectrumWallet.chainHash, 0L)

    if (ElectrumWallet.specs.contains(ewt.xPub) || secret.keys.bitcoinMaster == keys.bitcoinMaster) return
    val spec = ElectrumWallet.makeSigningWalletParts(core, ewt, Satoshi(0L), label = app getString bitcoin_wallet)
    btcWalletBag.addWallet(spec.info, ElectrumWallet.params.emptyPersistentDataBytes, spec.data.keys.ewt.xPub.publicKey)
    postInitBtcWallet(spec)
  }

  def initBtcWallet(info: CompleteBtcWalletInfo, ord: Long): Unit = {
    val ext = info.core.attachedMaster.getOrElse(secret.keys.bitcoinMaster)
    val ewt = ElectrumWalletType.makeSigningType(info.core.walletType, ext, chainHash, ord)
    val spec = ElectrumWallet.makeSigningWalletParts(info.core, ewt, info.lastBalance, info.label)
    ElectrumWallet.specs.update(ewt.xPub, spec)
    spec.walletRef ! info.initData
  }

  def postInitBtcWallet(spec: WalletSpec): Unit = {
    ElectrumWallet.specs.update(spec.data.keys.ewt.xPub, spec)
    spec.walletRef ! ElectrumWallet.params.emptyPersistentDataBytes
    ElectrumWallet.sync ! ElectrumWallet.ChainFor(spec.walletRef)
  }

  def removeBtcWallet(key: ExtendedPublicKey): Unit = {
    ElectrumWallet.specs.remove(key).foreach(_.walletRef ! PoisonPill)
    btcWalletBag.remove(key.publicKey)
  }

  def initWallets = {
    val (native, attached) = btcWalletBag.listWallets.partition(_.core.attachedMaster.isDefined)
    for (btcWalletInfo \ ord <- native.zipWithIndex) initBtcWallet(btcWalletInfo, ord)
    for (btcWalletInfo <- attached) initBtcWallet(btcWalletInfo, ord = 0L)

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

    // This is a single place where we should bypass sequential threading
    linkUsdt.data = LinkUsdt.WalletManager(usdtWalletBag.listWallets.toSet)

    if (linkUsdt.data.wallets.nonEmpty) {
      linkUsdt ! LinkUsdt.CmdEnsureUsdtAccounts
      linkUsdt ! WsListener.CmdConnect
    }

    if (showTaCard) {
      // This is a single place where we should bypass sequential threading
      for (status <- linkClient.loadUserStatus) linkClient.data = status
      linkClient ! WsListener.CmdConnect
    }
  }

  // Fiat conversion

  def currentRate(rates: Fiat2Btc, code: String): Try[Double] = Try(rates apply code)
  def msatInFiat(rates: Fiat2Btc, code: String)(msat: MilliSatoshi): Try[Double] = currentRate(rates, code).map(perBtc => msat.toLong * perBtc / BtcDenom.factor)
  val currentMsatInFiatHuman: MilliSatoshi => String = msat => msatInFiatHuman(fiatRates.info.rates, fiatCode, msat, Denomination.formatFiatShort)

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