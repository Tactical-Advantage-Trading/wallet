package trading.tacticaladvantage

import com.neovisionaries.ws.client._
import fr.acinq.bitcoin.Btc
import fr.acinq.eclair.{MilliSatoshi, ToMilliSatoshiConversion}
import immortan.Tools.{Any2Some, maxOptionByValue, minOptionByValue, none}
import immortan.sqlite.{DbStreams, SQLiteData}
import immortan.utils.ImplicitJsonFormats._
import immortan.utils.{BtcDenom, Rx}
import immortan.{CanBeShutDown, CoinDescription, StateMachine}
import spray.json._
import trading.tacticaladvantage.LinkClient._
import trading.tacticaladvantage.R.drawable._
import trading.tacticaladvantage.R.string._
import trading.tacticaladvantage.utils.WsListener._
import trading.tacticaladvantage.utils.{CoinUri, WsListener}

import java.util.Date
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

  val failureCodes = Seq(INVALID_JSON, NOT_AUTHORIZED, INVALID_REQUEST, UPDATE_CLIENT_APP, ACCOUNT_BANNED)

  implicit object FailureCodeFormat extends JsonFormat[FailureCode] {
    def write(fc: FailureCode): JsValue = JsNumber(fc.code)

    def read(value: JsValue): FailureCode = value match {
      case JsNumber(num) => failureCodes.find(_.code == num).get
      case _ => throw new RuntimeException
    }
  }

  // Deposit states

  sealed trait DepositState { def state: Int }
  case object Canceled extends DepositState { val state = 0 }
  case object Pending extends DepositState { val state = 1 }
  case object Done extends DepositState { val state = 2 }

  val despositStates: Seq[DepositState] = Seq(Canceled, Pending, Done)

  implicit object DepositStateFormat extends JsonFormat[DepositState] {
    def write(ds: DepositState): JsValue = JsNumber(ds.state)

    def read(value: JsValue): DepositState = value match {
      case JsNumber(num) => despositStates.find(_.state == num).get
      case _ => throw new RuntimeException
    }
  }

  // Withdraw types

  sealed trait WithdrawType { def code: Int }
  case object FullBalance extends WithdrawType { val code = 0 }
  case object PartialInterestNative extends WithdrawType { val code = 1 }
  case object PartialInterestUsd extends WithdrawType { val code = 2 }

  val withdrawTypes: Seq[WithdrawType] = Seq(FullBalance, PartialInterestNative, PartialInterestUsd)

  implicit object WithdrawTypeFormat extends JsonFormat[WithdrawType] {
    def write(ds: WithdrawType): JsValue = JsNumber(ds.code)

    def read(value: JsValue): WithdrawType = value match {
      case JsNumber(num) => withdrawTypes.find(_.code == num).get
      case _ => throw new RuntimeException
    }
  }

  // Request types

  sealed trait RequestArguments { val tag: String }

  case object GetLoanAd extends RequestArguments { val tag = "GetLoanAd" }

  case class GetUserStatus(sessionToken: String) extends RequestArguments { val tag = "GetUserStatus" }

  case class Login(oneTimePassword: Option[String], email: String) extends RequestArguments { val tag = "Login" }

  case class WithdrawReq(address: String, wt: WithdrawType) extends RequestArguments { val tag = "WithdrawReq" }

  case class DepositIntent(txid: String) extends RequestArguments { val tag = "DepositIntent" }

  case object CancelWithdraw extends RequestArguments { val tag = "CancelWithdraw" }

  case object GetHistory extends RequestArguments { val tag = "GetHistory" }

  implicit val getLoanAddFormat: JsonFormat[GetLoanAd.type] = taggedJsonFmt(jsonFormat0(construct = (/**/) => GetLoanAd), "GetLoanAd")
  implicit val loginFormat: JsonFormat[Login] = taggedJsonFmt(jsonFormat[Option[String], String, Login](Login.apply, "oneTimePassword", "email"), "Login")
  implicit val depositIntentFormat: JsonFormat[DepositIntent] = taggedJsonFmt(jsonFormat[String, DepositIntent](DepositIntent.apply, "txid"), "DepositIntent")
  implicit val withdrawReqFormat: JsonFormat[WithdrawReq] = taggedJsonFmt(jsonFormat[String, WithdrawType, WithdrawReq](WithdrawReq.apply, "address", "wt"), "Withdraw")
  implicit val getUserStatusFormat: JsonFormat[GetUserStatus] = taggedJsonFmt(jsonFormat[String, GetUserStatus](GetUserStatus.apply, "sessionToken"), "GetUserStatus")
  implicit val cancelWithdrawFormat: JsonFormat[CancelWithdraw.type] = taggedJsonFmt(jsonFormat0(construct = (/**/) => CancelWithdraw), "CancelWithdraw")
  implicit val getHistoryFormat: JsonFormat[GetHistory.type] = taggedJsonFmt(jsonFormat0(construct = (/**/) => GetHistory), "GetHistory")

  implicit object RequestArgumentsFormat extends JsonFormat[RequestArguments] {
    def write(obj: RequestArguments): JsValue = obj match {
      case request: Login => loginFormat.write(request)
      case request: WithdrawReq => withdrawReqFormat.write(request)
      case request: GetUserStatus => getUserStatusFormat.write(request)
      case request: DepositIntent => depositIntentFormat.write(request)
      case CancelWithdraw => cancelWithdrawFormat.write(CancelWithdraw)
      case GetHistory => getHistoryFormat.write(GetHistory)
      case GetLoanAd => getLoanAddFormat.write(GetLoanAd)
    }

    def read(json: JsValue): RequestArguments =
      throw new RuntimeException
  }

  // Response types

  case class TotalFunds(balance: BigDecimal, withdrawable: BigDecimal) {
    lazy val amountHuman = toHumanSum(withdrawable)
    lazy val currency = bitcoin_wallet
    lazy val icon = ic_logo_bitcoin_24
  }

  case class Deposit(txid: String, address: String, amount: BigDecimal, created: Long, state: DepositState)

  case class Withdraw(id: Long, txid: Option[String], exid: String, address: String, amount: BigDecimal, wt: Option[WithdrawType], created: Long, fee: BigDecimal) {
    lazy val nextWithdrawRes: Int = wt match { case Some(PartialInterestNative) => ta_withdraw_interest_when case _ => ta_withdraw_full_when }
  }

  case class ActiveLoan(id: Long, userId: Long, start: Long, end: Long, roi: BigDecimal, amount: BigDecimal) {
    lazy val daysLeft = Math.max(0L, (end - System.currentTimeMillis) / 86400000L)
    lazy val interestHuman = toHumanInSum(amount * roi / inverseSpan)
    lazy val inverseSpan = 365L * 86400000L / (end - start)
    lazy val amountHuman = toHumanSum(amount)
    lazy val icon = ic_logo_bitcoin_24
  }

  implicit val depositFormat: JsonFormat[Deposit] =
    jsonFormat[String, String, BigDecimal, Long, DepositState,
      Deposit](Deposit.apply, "txid", "address", "amount", "created", "state")

  implicit val withdrawFormat: JsonFormat[Withdraw] =
    jsonFormat[Long, Option[String], String, String, BigDecimal, Option[WithdrawType], Long, BigDecimal,
      Withdraw](Withdraw.apply, "id", "txid", "exid", "address", "amount", "wt", "created", "fee")

  implicit val activeLoanFormat: JsonFormat[ActiveLoan] =
    jsonFormat[Long, Long, Long, Long, BigDecimal, BigDecimal,
      ActiveLoan](ActiveLoan.apply, "id", "userId", "start", "end", "roi", "amount")

  implicit val totalFundsFormat: JsonFormat[TotalFunds] =
    jsonFormat[BigDecimal, BigDecimal, TotalFunds](TotalFunds.apply, "balance", "withdrawable")

  sealed trait ResponseArguments { def tag: String }

  case class Failure(failureCode: FailureCode) extends ResponseArguments { val tag = "Failure" }

  case class History(deposits: List[Deposit], withdraws: List[Withdraw], loans: List[ActiveLoan] = Nil) extends ResponseArguments { val tag = "History" }

  case class LoanAd(durationDays: Long, minDeposit: BigDecimal, maxDeposit: BigDecimal, address: String, challenge: String, roi: BigDecimal) extends ResponseArguments with CoinUri {
    val desc: CoinDescription = CoinDescription(List(address), WalletApp.app.getString(R.string.ta_btc_loan_label).trim.asSome, WalletApp.ID_BTC, taRoi = roi.asSome)
    val maxAmount: MilliSatoshi = Btc(maxDeposit).toSatoshi.toMilliSatoshi
    val amount: Option[MilliSatoshi] = None
    val tag = "LoanAd"
  }

  sealed trait TaLinkState
  case object LoggedOut extends TaLinkState
  case class UserStatus(pendingWithdraws: List[Withdraw], pendingDeposits: List[Deposit], activeLoans: List[ActiveLoan],
                        totalFunds: List[TotalFunds], email: String, sessionToken: String, withdrawDelay: Long) extends ResponseArguments with TaLinkState {
    val withdrawDate = new Date(maxOptionByValue(activeLoans)(_.end, 0L) max maxOptionByValue(pendingWithdraws)(_.created + withdrawDelay, 0L) max System.currentTimeMillis)
    val minLoanDaysLeft = minOptionByValue(activeLoans)(_.daysLeft, 0L).toInt
    val tag = "UserStatus"
  }

  implicit val loanAdFormat: JsonFormat[LoanAd] =
    taggedJsonFmt(jsonFormat[Long, BigDecimal, BigDecimal, String, String, BigDecimal,
      LoanAd](LoanAd.apply, "durationDays", "minDeposit", "maxDeposit", "address", "challenge", "roi"), "LoanAd")

  implicit val failureFormat: JsonFormat[Failure] = taggedJsonFmt(jsonFormat[FailureCode, Failure](Failure.apply, "failureCode"), "Failure")

  implicit val userStatusFormat: JsonFormat[UserStatus] =
    taggedJsonFmt(jsonFormat[List[Withdraw], List[Deposit], List[ActiveLoan], List[TotalFunds], String, String, Long,
      UserStatus](UserStatus.apply, "pendingWithdraws", "pendingDeposits", "activeLoans", "totalFunds", "email", "sessionToken", "withdrawDelay"), "UserStatus")

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

  def toHumanSum(amount: BigDecimal): String =
    BtcDenom.parsedTT(Btc(amount).toSatoshi.toMilliSatoshi,
      Colors.cardIn, Colors.cardZero)

  def toHumanInSum(amount: BigDecimal): String =
    BtcDenom.directedTT(Btc(amount).toSatoshi.toMilliSatoshi, MilliSatoshi(0L),
      Colors.cardOut, Colors.cardIn, Colors.cardZero, isIncoming = true)
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
      ws = factory.createSocket("wss://tactical-advantage.trading:8433")
      ws.addListener(wsListener).connectAsynchronously

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