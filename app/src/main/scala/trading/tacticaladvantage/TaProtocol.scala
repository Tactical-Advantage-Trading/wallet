package trading.tacticaladvantage

import spray.json._
import immortan.utils.ImplicitJsonFormats._

object TaProtocol {
  sealed trait FailureCode { def code: Int }
  case object INVALID_JSON extends FailureCode { val code = 10 }
  case object NOT_LOGGED_IN extends FailureCode { val code = 20 }
  case object NOT_AUTHORIZED extends FailureCode { val code = 30 }
  case object INVALID_REQUEST extends FailureCode { val code = 40 }
  case object UPDATE_CLIENT_APP extends FailureCode { val code = 50 }
  case object USD_INFRA_FAIL extends FailureCode { val code = 60 }
  case object ACCOUNT_BANNED extends FailureCode { val code = 70 }

  val failureCodes: Seq[FailureCode] = Seq(
    INVALID_JSON, NOT_LOGGED_IN, NOT_AUTHORIZED, INVALID_REQUEST,
    UPDATE_CLIENT_APP, USD_INFRA_FAIL, ACCOUNT_BANNED
  )

  def fromCode(code: Int): Option[FailureCode] =
    failureCodes.find(_.code == code)

  implicit object FailureCodeFormat extends JsonFormat[FailureCode] {
    def write(fc: FailureCode): JsValue = JsNumber(fc.code)

    def read(value: JsValue): FailureCode = value match {
      case JsNumber(num) => fromCode(num.toInt).get
      case _ => throw new RuntimeException
    }
  }

  //

  sealed trait Asset { def kind: Int }
  case object BTC extends Asset { val kind = 0 }
  case object USD extends Asset { val kind = 1 }

  val assets: Seq[Asset] = Seq(BTC, USD)
  def fromKind(kind: Int): Option[Asset] =
    assets.find(_.kind == kind)

  implicit object AssetFormat extends JsonFormat[Asset] {
    def write(a: Asset): JsValue = JsNumber(a.kind)

    def read(value: JsValue): Asset = value match {
      case JsNumber(num) => fromKind(num.toInt).get
      case _ => throw new RuntimeException
    }
  }

  //

  sealed trait RequestArguments { val tag: String }
  case class Login(oneTimePassword: Option[String], email: String) extends RequestArguments { val tag = "Login" }
  case class UsdSubscribe(addresses: List[String], afterBlock: Long) extends RequestArguments { val tag = "UsdSubscribe" }
  case class WithdrawReq(address: String, requested: Double, asset: Asset) extends RequestArguments { val tag = "WithdrawReq" }
  case class DepositSig(bip322: String, asset: Asset) extends RequestArguments { val tag = "DepositSig" }
  case class CancelWithdraw(asset: Asset) extends RequestArguments { val tag = "CancelWithdraw" }
  case object GetHistory extends RequestArguments { val tag = "GetHistory" }
  case object LogOut extends RequestArguments { val tag = "LogOut" }

  implicit val loginFormat: JsonFormat[Login] = taggedJsonFmt(jsonFormat2(Login), "Login")
  implicit val usdSubscribeFormat: JsonFormat[UsdSubscribe] = taggedJsonFmt(jsonFormat2(UsdSubscribe), "UsdSubscribe")
  implicit val withdrawReqFormat: JsonFormat[WithdrawReq] = taggedJsonFmt(jsonFormat3(WithdrawReq), "Withdraw")
  implicit val depositSigFormat: JsonFormat[DepositSig] = taggedJsonFmt(jsonFormat2(DepositSig), "DepositSig")
  implicit val cancelWithdrawFormat: JsonFormat[CancelWithdraw] = taggedJsonFmt(jsonFormat1(CancelWithdraw), "CancelWithdraw")
  implicit val getHistoryFormat: JsonFormat[GetHistory.type] = taggedJsonFmt(jsonFormat0(construct = (/**/) => GetHistory), "GetHistory")
  implicit val logOutFormat: JsonFormat[LogOut.type] = taggedJsonFmt(jsonFormat0(construct = (/**/) => LogOut), "LogOut")

  implicit object RequestArgumentsFormat extends JsonFormat[RequestArguments] {
    def write(obj: RequestArguments): JsValue = obj match {
      case request: Login => loginFormat.write(request)
      case request: CancelWithdraw => cancelWithdrawFormat.write(request)
      case request: UsdSubscribe => usdSubscribeFormat.write(request)
      case request: WithdrawReq => withdrawReqFormat.write(request)
      case request: DepositSig => depositSigFormat.write(request)
      case GetHistory => getHistoryFormat.write(GetHistory)
      case LogOut => logOutFormat.write(LogOut)
    }

    def read(json: JsValue): RequestArguments =
      throw new RuntimeException
  }

  //

  case class TotalFunds(balance: Double, withdrawable: Double)
  case class ActiveLoan(id: Long, userId: Long, start: Long, end: Long, roi: Double, amount: Double, asset: Asset)
  case class Deposit(txid: String, address: String, amount: Double, created: Long, isConfirmed: Boolean, isCanceled: Boolean, asset: Asset)
  case class Withdraw(txid: Option[String], id: String, address: String, amount: Double, requested: Double, created: Long, fee: Double, asset: Asset)
  case class UsdTransfer(amount: String, fromAddr: String, toAddr: String, hash: String, block: Long, stamp: Long, isRemoved: Boolean)

  implicit val depositFormat: JsonFormat[Deposit] = jsonFormat7(Deposit)
  implicit val withdrawFormat: JsonFormat[Withdraw] = jsonFormat8(Withdraw)
  implicit val activeLoanFormat: JsonFormat[ActiveLoan] = jsonFormat7(ActiveLoan)
  implicit val totalFundsFormat: JsonFormat[TotalFunds] = jsonFormat2(TotalFunds)
  implicit val usdTransferFormat: JsonFormat[UsdTransfer] = jsonFormat7(UsdTransfer)

  sealed trait ResponseArguments { def tag: String }
  case class Failure(failureCode: FailureCode) extends ResponseArguments { val tag = "Failure" }
  case class UsdTransfers(transfers: List[UsdTransfer], chainTip: Long) extends ResponseArguments { val tag = "UsdTransfers" }
  case class UsdBalanceNonce(address: String, balance: String, nonce: String) extends ResponseArguments { val tag = "UsdBalanceNonce" }
  case class LoanAd(durationDays: Long, minDeposit: Double, maxDeposit: Double, address: Option[String], challenge: String, roi: Double, asset: Asset) extends ResponseArguments { val tag = "LoanAd" }
  case class UserStatus(pendingWithdraws: List[Withdraw], activeLoans: List[ActiveLoan], totalFunds: List[TotalFunds], email: String) extends ResponseArguments { val tag = "UserStatus" }
  case class History(deposits: List[Deposit], withdraws: List[Withdraw], loans: List[ActiveLoan] = Nil) extends ResponseArguments { val tag = "History" }

  implicit val loanAdFormat: JsonFormat[LoanAd] = taggedJsonFmt(jsonFormat7(LoanAd), "LoanAd")
  implicit val failureFormat: JsonFormat[Failure] = taggedJsonFmt(jsonFormat1(Failure), "Failure")
  implicit val usdTransfersFormat: JsonFormat[UsdTransfers] = taggedJsonFmt(jsonFormat2(UsdTransfers), "UsdTransfers")
  implicit val usdBalanceNonceFormat: JsonFormat[UsdBalanceNonce] = taggedJsonFmt(jsonFormat3(UsdBalanceNonce), "UsdBalanceNonce")
  implicit val userStatusFormat: JsonFormat[UserStatus] = taggedJsonFmt(jsonFormat4(UserStatus), "UserStatus")
  implicit val historyFormat: JsonFormat[History] = taggedJsonFmt(jsonFormat3(History), "History")

  implicit object ResponseArgumentsFormat extends JsonFormat[ResponseArguments] {
    def read(json: JsValue): ResponseArguments = json.asJsObject.fields(TAG) match {
      case JsString("UsdBalanceNonce") => usdBalanceNonceFormat.read(json)
      case JsString("UsdTransfers") => usdTransfersFormat.read(json)
      case JsString("UserStatus") => userStatusFormat.read(json)
      case JsString("Failure") => failureFormat.read(json)
      case JsString("History") => historyFormat.read(json)
      case JsString("LoanAd") => loanAdFormat.read(json)
      case _ => throw new RuntimeException
    }

    def write(obj: ResponseArguments): JsValue =
      throw new RuntimeException
  }

  //

  case class Request(version: Int, arguments: RequestArguments, sessionSecret: Option[String], id: String)
  case class Response(arguments: Option[ResponseArguments], newSessionSecret: Option[String], id: String)

  implicit val requestFormat : JsonFormat[Request] = jsonFormat4(Request)
  implicit val responseFormat: JsonFormat[Response] = jsonFormat3(Response)
}
