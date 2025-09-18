package trading.tacticaladvantage

import com.neovisionaries.ws.client._
import immortan.{CanBeShutDown, StateMachine, UsdtDescription}
import immortan.Tools.none
import immortan.sqlite.{CompleteUsdtWalletInfo, DbStreams, SQLiteUsdtWallet}
import immortan.utils.ImplicitJsonFormats._
import immortan.utils.Rx
import spray.json._
import trading.tacticaladvantage.LinkUsdt._
import trading.tacticaladvantage.utils.WsListener
import trading.tacticaladvantage.utils.WsListener._

object LinkUsdt {
  val USDT_UPDATE: String = "usdt-update"
  val GENERAL_ERROR: String = "general-error"
  val VERSION: Char = '1'

  case class WalletManager(wallets: Set[CompleteUsdtWalletInfo] = Set.empty) {
    lazy val withRealAddress: Set[CompleteUsdtWalletInfo] = wallets.filterNot(_.lcAddress == CompleteUsdtWalletInfo.NOADDRESS)
    lazy val okWallets: Map[String, CompleteUsdtWalletInfo] = withRealAddress.map(wallet => wallet.lcAddress -> wallet).toMap
    lazy val totalBalance: BigDecimal = withRealAddress.map(_.lastBalanceDecimal).sum
  }

  // Failure codes

  sealed trait FailureCode { def code: Int }
  case object INVALID_JSON extends FailureCode { val code = 10 }
  case object INVALID_REQUEST extends FailureCode { val code = 20 }
  case object UPDATE_CLIENT_APP extends FailureCode { val code = 30 }
  case object INFRA_FAIL extends FailureCode { val code = 40 }

  val failureCodes =
    Seq(INVALID_JSON, INVALID_REQUEST,
      UPDATE_CLIENT_APP, INFRA_FAIL)

  implicit object FailureCodeFormat extends JsonFormat[FailureCode] {
    def write(fc: FailureCode): JsValue = JsNumber(fc.code)

    def read(value: JsValue): FailureCode = value match {
      case JsNumber(num) => failureCodes.find(_.code == num).get
      case _ => throw new RuntimeException
    }
  }

  // Request types

  case class UsdtSubscribe(address: String, afterBlock: Long) { val tag = "UsdtSubscribe" }

  case class UsdtTransfer(amount: String, fromAddr: String, toAddr: String, hash: String, block: Long, stamp: Long, isRemoved: Boolean) {
    lazy val desc: UsdtDescription = UsdtDescription(fromAddr, toAddr)
  }

  implicit val usdSubscribeFormat: JsonFormat[UsdtSubscribe] = taggedJsonFmt(jsonFormat[String, Long,
    UsdtSubscribe](UsdtSubscribe.apply, "address", "afterBlock"), "UsdtSubscribe")

  implicit val usdtTransferFormat: JsonFormat[UsdtTransfer] = jsonFormat[String, String, String, String, Long, Long, Boolean,
    UsdtTransfer](UsdtTransfer.apply, "amount", "fromAddr", "toAddr", "hash", "block", "stamp", "isRemoved")

  // Response types

  sealed trait ResponseArguments { def tag: String }

  case class UsdtFailure(failureCode: FailureCode) extends ResponseArguments { val tag = "UsdtFailure" }

  case class UsdtTransfers(transfers: List[UsdtTransfer] = Nil) extends ResponseArguments {
    val olderFirstTransfers: List[UsdtTransfer] = transfers.sortBy(_.stamp)
    val tag = "UsdtTransfers"
  }

  case class UsdtBalanceNonce(address: String, balance: String, nonce: String) extends ResponseArguments { val tag = "UsdtBalanceNonce" }

  implicit val usdtFailureFormat: JsonFormat[UsdtFailure] = taggedJsonFmt(jsonFormat[FailureCode,
    UsdtFailure](UsdtFailure.apply, "failureCode"), "UsdtFailure")

  implicit val usdtTransfersFormat: JsonFormat[UsdtTransfers] = taggedJsonFmt(jsonFormat[List[UsdtTransfer],
    UsdtTransfers](UsdtTransfers.apply, "transfers"), "UsdtTransfers")

  implicit val usdtBalanceNonceFormat: JsonFormat[UsdtBalanceNonce] = taggedJsonFmt(jsonFormat[String, String, String,
    UsdtBalanceNonce](UsdtBalanceNonce.apply, "address", "balance", "nonce"), "UsdtBalanceNonce")

  implicit object ResponseArgumentsFormat extends JsonFormat[ResponseArguments] {
    def read(json: JsValue): ResponseArguments = json.asJsObject.fields(TAG) match {
      case JsString("UsdtBalanceNonce") => usdtBalanceNonceFormat.read(json)
      case JsString("UsdtTransfers") => usdtTransfersFormat.read(json)
      case JsString("UsdtFailure") => usdtFailureFormat.read(json)
      case _ => throw new RuntimeException
    }

    def write(obj: ResponseArguments): JsValue =
      throw new RuntimeException
  }

  case class Request(arguments: UsdtSubscribe, id: String)

  case class Response(arguments: Option[ResponseArguments], id: String)

  implicit val requestFormat: JsonFormat[Request] = jsonFormat[UsdtSubscribe, String, Request](Request.apply, "arguments", "id")
  implicit val responseFormat: JsonFormat[Response] = jsonFormat[Option[ResponseArguments], String, Response](Response.apply, "arguments", "id")

  class Listener(val id: String) {
    def onResponse(args: Option[ResponseArguments] = None): Unit = none
    def onChainTip(chainTip: Long): Unit = none
    def onDisconnected: Unit = none
    def onConnected: Unit = none
  }

  case class CmdRemove(listener: Listener)

  case class CmdRemoveWallet(xPriv: String)

  case object CmdEnsureUsdtAccounts
}

class LinkUsdt(usdtWalletBag: SQLiteUsdtWallet, biconomy: Biconomy) extends StateMachine[WalletManager] with CanBeShutDown { me =>
  val wsListener = new WsListener[WalletManager, Response](me, tryTo[Response], println)
  var listeners = List.empty[Listener]
  var ws: WebSocket = _

  def becomeShutDown: Unit = {
    ws.removeListener(wsListener)
    state = DISCONNECTED
    ws.disconnect
  }

  def !!(change: Any): Unit = (change, state) match {
    case (listener: Listener, _) => listeners = listeners :+ listener
    case (CmdRemove(listener), _) => listeners = listeners diff List(listener)

    case (CmdRemoveWallet(xPriv), _) =>
      val wallets1 = data.wallets.filterNot(_.xPriv == xPriv)
      data = data.copy(wallets = wallets1)
      usdtWalletBag.remove(xPriv)

    case (info: CompleteUsdtWalletInfo, _) =>
      data = data.copy(wallets = data.wallets - info + info)
      usdtWalletBag.addUpdateWallet(info)

    case (UsdtBalanceNonce(address, balance, nonce), _) =>
      data.wallets.find(_.lcAddress == address).foreach { walletInfo =>
        me !! walletInfo.copy(lastBalance = balance, lastNonce = nonce)
        DbStreams.next(DbStreams.txStream)
      }

    case (CmdEnsureUsdtAccounts, _) =>
      data.wallets.find(_.lcAddress == CompleteUsdtWalletInfo.NOADDRESS).foreach { walletInfo =>
        biconomy.getSmartAccountAddress(privKey = walletInfo.xPriv).foreach { response =>
          val sub = UsdtSubscribe(response.smartAccountAddress.toLowerCase, 0L)
          me !! walletInfo.copy(address = response.smartAccountAddress)
          me ! Request(sub, USDT_UPDATE)
        }
      }

    case (bin: BinaryMessage, _) =>
      for (chainTip <- bin.asLongTry) {
        for (info <- data.wallets) me !! info.copy(chainTip = chainTip)
        for (listener <- listeners) listener.onChainTip(chainTip)
      }

    case (CmdConnect, DISCONNECTED) =>
      val factory = (new WebSocketFactory).setConnectionTimeout(10000)
      ws = factory.createSocket("wss://tactical-advantage.trading:8080")
      ws.addListener(wsListener).connectAsynchronously

    case (CmdDisconnected, _) if !ws.isOpen =>
      Rx.delay(3000).foreach(_ => me ! CmdConnect)
      listeners.foreach(_.onDisconnected)
      become(data, DISCONNECTED)

    case (CmdConnected, DISCONNECTED) =>
      listeners.foreach(_.onConnected)
      become(data, CONNECTED)

    case (req: Request, CONNECTED) => ws.sendText(s"$VERSION${req.toJson.compactPrint}")
    case (req: Request, DISCONNECTED) => listeners.filter(_.id == req.id).foreach(_.onDisconnected)
    case (Response(arguments, id), CONNECTED) => listeners.filter(_.id == id).foreach(_ onResponse arguments)
    case _ =>
  }

  data = WalletManager(Set.empty)
  state = DISCONNECTED
}