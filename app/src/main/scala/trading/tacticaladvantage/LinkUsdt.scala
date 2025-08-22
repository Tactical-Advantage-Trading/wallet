package trading.tacticaladvantage

import com.neovisionaries.ws.client._
import immortan.UsdtDescription
import immortan.crypto.Tools.{ThrowableOps, none}
import immortan.crypto.{CanBeShutDown, StateMachine}
import immortan.sqlite.{CompleteUsdtWalletInfo, SQLiteData, SQLiteUsdtWallet}
import immortan.utils.ImplicitJsonFormats._
import immortan.utils.Rx
import spray.json._

import scala.util.Try
import trading.tacticaladvantage.LinkUsdt._
import trading.tacticaladvantage.utils.WsListener
import trading.tacticaladvantage.utils.WsListener._

object LinkUsdt {
  val USDT_UPDATE: String = "usdt-update"
  val GENERAL_ERROR: String = "general-error"
  val VERSION: Char = '1'

  case class WalletManager(wallets: Set[CompleteUsdtWalletInfo] = Set.empty) {
    lazy val withRealAddress: Set[CompleteUsdtWalletInfo] = wallets.filterNot(_.address == CompleteUsdtWalletInfo.NOADDRESS)
    lazy val okWallets: Map[String, CompleteUsdtWalletInfo] = withRealAddress.map(wallet => wallet.address -> wallet).toMap
    lazy val totalBalance: Double = withRealAddress.map(_.lastBalance.toDouble).sum
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
  case class UsdtTransfers(transfers: List[UsdtTransfer], chainTip: Long) extends ResponseArguments { val tag = "UsdtTransfers" }
  case class UsdtBalanceNonce(address: String, balance: String, nonce: String, chainTip: Long) extends ResponseArguments { val tag = "UsdtBalanceNonce" }

  implicit val usdtFailureFormat: JsonFormat[UsdtFailure] = taggedJsonFmt(jsonFormat[FailureCode,
    UsdtFailure](UsdtFailure.apply, "failureCode"), "UsdtFailure")

  implicit val usdtTransfersFormat: JsonFormat[UsdtTransfers] = taggedJsonFmt(jsonFormat[List[UsdtTransfer], Long,
    UsdtTransfers](UsdtTransfers.apply, "transfers", "chainTip"), "UsdtTransfers")

  implicit val usdtBalanceNonceFormat: JsonFormat[UsdtBalanceNonce] = taggedJsonFmt(jsonFormat[String, String, String, Long,
    UsdtBalanceNonce](UsdtBalanceNonce.apply, "address", "balance", "nonce", "chainTip"), "UsdtBalanceNonce")

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
    def onDisconnected: Unit = none
    def onConnected: Unit = none
  }

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

    case (info: CompleteUsdtWalletInfo, _) =>
      val data1 = data.copy(data.wallets - info + info)
      usdtWalletBag.addUpdateWallet(info)
      become(data1, state)

    case (UsdtBalanceNonce(address, balance, nonce, tip), _) =>
      data.wallets.find(_.address == address).foreach { walletInfo =>
        me !! walletInfo.copy(lastBalance = balance, lastNonce = nonce, chainTip = tip)
      }

    case (CmdEnsureUsdtAccounts, _) =>
      data.wallets.find(_.address == CompleteUsdtWalletInfo.NOADDRESS).foreach { walletInfo =>
        biconomy.getSmartAccountAddress(privKey = walletInfo.xPriv).foreach { response =>
          me ! Request(UsdtSubscribe(response.smartAccountAddress, 0L), USDT_UPDATE)
          me !! walletInfo.copy(address = response.smartAccountAddress)
        }
      }

    case (CmdConnect, DISCONNECTED) =>
      val endpoint = "wss://tactical-advantage.trading/usdt"
      val factory = (new WebSocketFactory).setConnectionTimeout(10000)
      ws = factory.createSocket(endpoint, 443).addListener(wsListener)
      ws.connectAsynchronously

    case (CmdDisconnected, CONNECTED) =>
      Rx.delay(5000).foreach(_ => me ! CmdConnect)
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