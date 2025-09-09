package trading.tacticaladvantage

import com.neovisionaries.ws.client._
import fr.acinq.bitcoin.Btc
import fr.acinq.eclair.{MilliSatoshi, ToMilliSatoshiConversion}
import immortan.Tools.{Any2Some, none}
import immortan.sqlite.{DbStreams, SQLiteData}
import immortan.utils.ImplicitJsonFormats._
import immortan.utils.{BtcDenom, Denomination, Rx}
import immortan.{BtcDescription, CanBeShutDown, PlainBtcDescription, StateMachine}
import spray.json._
import trading.tacticaladvantage.LinkClient._
import trading.tacticaladvantage.utils.WsListener._
import trading.tacticaladvantage.utils.{BitcoinUri, WsListener}

import scala.util.Try

object LinkClient {
  val USER_UPDATE: String = "user-update"
  val GENERAL_ERROR: String = "general-error"
  val VERSION: Char = '1'

  // Failure codes

  sealed trait FailureCode { def code: Int }
  case object INVALID_JSON extends FailureCode { val code = 10 }
  case object NOT_AUTHORIZED extends FailureCode { val code = 20 }
  case object INVALID_REQUEST extends FailureCode { val code = 30 }
  case object UPDATE_CLIENT_APP extends FailureCode { val code = 40 }
  case object ACCOUNT_BANNED extends FailureCode { val code = 50 }

  val failureCodes =
    Seq(INVALID_JSON, NOT_AUTHORIZED,
      INVALID_REQUEST, UPDATE_CLIENT_APP,
      ACCOUNT_BANNED)

  implicit object FailureCodeFormat extends JsonFormat[FailureCode] {
    def write(fc: FailureCode): JsValue = JsNumber(fc.code)

    def read(value: JsValue): FailureCode = value match {
      case JsNumber(num) => failureCodes.find(_.code == num).get
      case _ => throw new RuntimeException
    }
  }

  // Asset types

  sealed trait Asset { def kind: Int }
  case object BTC extends Asset { val kind = 0 }
  case object USD extends Asset { val kind = 1 }

  val assets: Seq[Asset] = Seq(BTC, USD)

  implicit object AssetFormat extends JsonFormat[Asset] {
    def write(a: Asset): JsValue = JsNumber(a.kind)

    def read(value: JsValue): Asset = value match {
      case JsNumber(num) => assets.find(_.kind == num).get
      case _ => throw new RuntimeException
    }
  }

  // Request types

  sealed trait RequestArguments { val tag: String }

  case class GetLoanAd(asset: Asset) extends RequestArguments { val tag = "GetLoanAd" }

  case class GetUserStatus(sessionToken: String) extends RequestArguments { val tag = "GetUserStatus" }

  case class Login(oneTimePassword: Option[String], email: String) extends RequestArguments { val tag = "Login" }

  case class WithdrawReq(address: String, asset: Asset, requested: Option[BigDecimal] = None) extends RequestArguments { val tag = "WithdrawReq" }

  case class DepositIntent(txid: String, asset: Asset) extends RequestArguments { val tag = "DepositIntent" }

  case class CancelWithdraw(asset: Asset) extends RequestArguments { val tag = "CancelWithdraw" }

  case object GetHistory extends RequestArguments { val tag = "GetHistory" }

  implicit val getLoanAddFormat: JsonFormat[GetLoanAd] = taggedJsonFmt(jsonFormat[Asset, GetLoanAd](GetLoanAd.apply, "asset"), "GetLoanAd")
  implicit val loginFormat: JsonFormat[Login] = taggedJsonFmt(jsonFormat[Option[String], String, Login](Login.apply, "oneTimePassword", "email"), "Login")
  implicit val withdrawReqFormat: JsonFormat[WithdrawReq] = taggedJsonFmt(jsonFormat[String, Asset, Option[BigDecimal], WithdrawReq](WithdrawReq.apply, "address", "asset", "requested"), "Withdraw")
  implicit val depositIntentFormat: JsonFormat[DepositIntent] = taggedJsonFmt(jsonFormat[String, Asset, DepositIntent](DepositIntent.apply, "txid", "asset"), "DepositIntent")
  implicit val getUserStatusFormat: JsonFormat[GetUserStatus] = taggedJsonFmt(jsonFormat[String, GetUserStatus](GetUserStatus.apply, "sessionToken"), "GetUserStatus")
  implicit val cancelWithdrawFormat: JsonFormat[CancelWithdraw] = taggedJsonFmt(jsonFormat[Asset, CancelWithdraw](CancelWithdraw.apply, "asset"), "CancelWithdraw")
  implicit val getHistoryFormat: JsonFormat[GetHistory.type] = taggedJsonFmt(jsonFormat0(construct = (/**/) => GetHistory), "GetHistory")

  implicit object RequestArgumentsFormat extends JsonFormat[RequestArguments] {
    def write(obj: RequestArguments): JsValue = obj match {
      case request: Login => loginFormat.write(request)
      case request: GetLoanAd => getLoanAddFormat.write(request)
      case request: GetUserStatus => getUserStatusFormat.write(request)
      case request: CancelWithdraw => cancelWithdrawFormat.write(request)
      case request: DepositIntent => depositIntentFormat.write(request)
      case request: WithdrawReq => withdrawReqFormat.write(request)
      case GetHistory => getHistoryFormat.write(GetHistory)
    }

    def read(json: JsValue): RequestArguments =
      throw new RuntimeException
  }

  // Response types

  case class TotalFunds(balance: BigDecimal, withdrawable: BigDecimal, asset: Asset) {
    lazy val amountHuman = toHumanSum(asset, withdrawable)
    lazy val currency = toTextRes(asset)
    lazy val icon = toIconRes(asset)
  }

  case class Deposit(txid: String, address: String, amount: BigDecimal, created: Long, isConfirmed: Boolean, isCanceled: Boolean, asset: Asset)

  case class Withdraw(txid: Option[String], id: String, address: String, amount: BigDecimal, requested: BigDecimal, created: Long, fee: BigDecimal, asset: Asset)

  case class ActiveLoan(id: Long, userId: Long, start: Long, end: Long, roi: BigDecimal, amount: BigDecimal, asset: Asset) {
    lazy val daysLeft = Math.max(0L, (end - System.currentTimeMillis) / 86400000L)
    lazy val interestHuman = toHumanInSum(asset, amount * roi / inverseSpan)
    lazy val inverseSpan = 365L * 86400000L / (end - start)
    lazy val amountHuman = toHumanSum(asset, amount)
    lazy val icon = toIconRes(asset)
  }

  implicit val depositFormat: JsonFormat[Deposit] =
    jsonFormat[String, String, BigDecimal, Long, Boolean, Boolean, Asset,
      Deposit](Deposit.apply, "txid", "address", "amount", "created", "isConfirmed", "isCanceled", "asset")

  implicit val withdrawFormat: JsonFormat[Withdraw] =
    jsonFormat[Option[String], String, String, BigDecimal, BigDecimal, Long, BigDecimal, Asset,
      Withdraw](Withdraw.apply, "txid", "id", "address", "amount", "requested", "created", "fee", "asset")

  implicit val activeLoanFormat: JsonFormat[ActiveLoan] =
    jsonFormat[Long, Long, Long, Long, BigDecimal, BigDecimal, Asset,
      ActiveLoan](ActiveLoan.apply, "id", "userId", "start", "end", "roi", "amount", "asset")

  implicit val totalFundsFormat: JsonFormat[TotalFunds] =
    jsonFormat[BigDecimal, BigDecimal, Asset, TotalFunds](TotalFunds.apply, "balance", "withdrawable", "asset")

  sealed trait ResponseArguments { def tag: String }

  case class Failure(failureCode: FailureCode) extends ResponseArguments { val tag = "Failure" }

  case class History(deposits: List[Deposit], withdraws: List[Withdraw], loans: List[ActiveLoan] = Nil) extends ResponseArguments { val tag = "History" }

  case class LoanAd(durationDays: Long, minDeposit: BigDecimal, maxDeposit: BigDecimal, address: String, challenge: String, roi: BigDecimal, asset: Asset) extends ResponseArguments with BitcoinUri {
    val label: Option[String] = WalletApp.app.getString(R.string.ta_btc_loan_label).trim.asSome
    val desc: BtcDescription = PlainBtcDescription(List(address), label, taRoi = roi.asSome)
    val maxAmount: MilliSatoshi = Btc(maxDeposit).toSatoshi.toMilliSatoshi
    val amount: Option[MilliSatoshi] = None
    val tag = "LoanAd"
  }

  sealed trait TaLinkState

  case object LoggedOut extends TaLinkState

  case class UserStatus(pendingWithdraws: List[Withdraw], activeLoans: List[ActiveLoan], totalFunds: List[TotalFunds],
                        email: String, sessionToken: String) extends ResponseArguments with TaLinkState { val tag = "UserStatus" }

  implicit val loanAdFormat: JsonFormat[LoanAd] =
    taggedJsonFmt(jsonFormat[Long, BigDecimal, BigDecimal, String, String, BigDecimal, Asset,
      LoanAd](LoanAd.apply, "durationDays", "minDeposit", "maxDeposit", "address", "challenge", "roi", "asset"), "LoanAd")

  implicit val failureFormat: JsonFormat[Failure] = taggedJsonFmt(jsonFormat[FailureCode,
    Failure](Failure.apply, "failureCode"), "Failure")

  implicit val userStatusFormat: JsonFormat[UserStatus] =
    taggedJsonFmt(jsonFormat[List[Withdraw], List[ActiveLoan], List[TotalFunds], String, String,
      UserStatus](UserStatus.apply, "pendingWithdraws", "activeLoans", "totalFunds", "email", "sessionToken"), "UserStatus")

  implicit val historyFormat: JsonFormat[History] =
    taggedJsonFmt(jsonFormat[List[Deposit], List[Withdraw], List[ActiveLoan],
      History](History.apply, "deposits", "withdraws", "loans"), "History")

  implicit object ResponseArgumentsFormat extends JsonFormat[ResponseArguments] {
    def read(json: JsValue): ResponseArguments = json.asJsObject.fields(TAG) match {
      case JsString("UserStatus") => userStatusFormat.read(json)
      case JsString("Failure") => failureFormat.read(json)
      case JsString("History") => historyFormat.read(json)
      case JsString("LoanAd") => loanAdFormat.read(json)
      case _ => throw new RuntimeException
    }

    def write(obj: ResponseArguments): JsValue =
      throw new RuntimeException
  }

  case class Request(arguments: RequestArguments, id: String)

  case class Response(arguments: Option[ResponseArguments], id: String)

  implicit val requestFormat: JsonFormat[Request] = jsonFormat[RequestArguments, String, Request](Request.apply, "arguments", "id")

  implicit val responseFormat: JsonFormat[Response] = jsonFormat[Option[ResponseArguments], String, Response](Response.apply, "arguments", "id")

  class Listener(val id: String) {
    def onResponse(args: Option[ResponseArguments] = None): Unit = none
    def onConnected(stateData: TaLinkState): Unit = none
    def onDisconnected: Unit = none
  }

  case class CmdRemove(listener: Listener)

  def toHumanSum(asset: Asset, amount: BigDecimal): String = asset match {
    case BTC => BtcDenom.parsedTT(Btc(amount).toSatoshi.toMilliSatoshi, Colors.cardIn, Colors.cardZero)
    case USD => Denomination.fiatTT("0", amount.toString, null, Colors.cardIn, isIncoming = false)
  }

  def toHumanInSum(asset: Asset, amount: BigDecimal): String = asset match {
    case BTC => BtcDenom.directedTT(Btc(amount).toSatoshi.toMilliSatoshi, MilliSatoshi(0L), Colors.cardOut, Colors.cardIn, Colors.cardZero, isIncoming = true)
    case USD => Denomination.fiatDirectedTT(incoming = amount.toString, outgoing = "0", Colors.cardOut, Colors.cardIn, isIncoming = true)
  }

  def toTextRes(asset: Asset): Int = asset match {
    case BTC => trading.tacticaladvantage.R.string.bitcoin_wallet
    case USD => trading.tacticaladvantage.R.string.usdt_wallet
  }

  def toIconRes(asset: Asset): Int = asset match {
    case BTC => trading.tacticaladvantage.R.drawable.ic_logo_bitcoin_24
    case USD => trading.tacticaladvantage.R.drawable.ic_logo_tether_24
  }
}

class LinkClient(extDataBag: SQLiteData) extends StateMachine[TaLinkState] with CanBeShutDown { me =>
  val wsListener = new WsListener[TaLinkState, Response](me, tryTo[Response], println)
  var listeners = List.empty[Listener]
  var ws: WebSocket = _

  def becomeShutDown: Unit = {
    ws.removeListener(wsListener)
    state = DISCONNECTED
    data = LoggedOut
    ws.disconnect
  }

  def !!(change: Any): Unit = (change, state) match {
    case (listener: Listener, _) => listeners = listeners :+ listener
    case (CmdRemove(listener), _) => listeners = listeners diff List(listener)

    case (LoggedOut, _) =>
      extDataBag.delete(TA_USER_STATUS)
      become(freshData = LoggedOut, state)
      DbStreams.next(DbStreams.walletStream)

    case (status: UserStatus, _) =>
      saveUserStatus(newStatus = status)
      become(freshData = status, freshState = state)
      DbStreams.next(DbStreams.walletStream)

    case (CmdConnect, DISCONNECTED) =>
      val factory = (new WebSocketFactory).setConnectionTimeout(10000)
      ws = factory.createSocket("ws://10.0.2.2:8433").addListener(wsListener)
      ws.connectAsynchronously

    case (CmdDisconnected, _) if !ws.isOpen =>
      Rx.delay(3000).foreach(_ => me ! CmdConnect)
      listeners.foreach(_.onDisconnected)
      become(data, DISCONNECTED)

    case (CmdConnected, DISCONNECTED) =>
      listeners.foreach(_ onConnected data)
      become(data, CONNECTED)

    case (req: Request, CONNECTED) => ws.sendText(s"$VERSION${req.toJson.compactPrint}")
    case (req: Request, DISCONNECTED) => listeners.filter(_.id == req.id).foreach(_.onDisconnected)
    case (Response(arguments, id), CONNECTED) => listeners.filter(_.id == id).foreach(_ onResponse arguments)
    case _ =>
  }

  private val TA_USER_STATUS = "ta-user-status"
  def loadUserStatus: Try[UserStatus] = extDataBag.tryGet(TA_USER_STATUS).map(SQLiteData.byteVecToString) map to[UserStatus]
  def saveUserStatus(newStatus: UserStatus): Unit = extDataBag.put(TA_USER_STATUS, newStatus.toJson.compactPrint getBytes "UTF-8")

  state = DISCONNECTED
  data = LoggedOut
}