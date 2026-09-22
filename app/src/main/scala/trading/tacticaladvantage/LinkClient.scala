package trading.tacticaladvantage

import com.neovisionaries.ws.client._
import fr.acinq.bitcoin.Btc
import fr.acinq.eclair.{MilliSatoshi, ToMilliSatoshiConversion}
import spray.json._
import trading.tacticaladvantage.LinkClient._
import trading.tacticaladvantage.Tools.{Any2Some, maxOptionByValue, minOptionByValue, none}
import trading.tacticaladvantage.sqlite.{DbStreams, SQLiteData}
import trading.tacticaladvantage.utils.ImplicitJsonFormats._
import trading.tacticaladvantage.utils.WsListener._
import trading.tacticaladvantage.utils.{CoinUri, Rx, WsListener}

import java.util.Date
import scala.util.Try

object LinkClient {
  val ALL_IDS: String = "all-ids"
  val USER_UPDATE: String = "user-update"
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

  // Asset types

  sealed trait AssetType { def kind: String }
  case object BTC extends AssetType { val kind = "BTC" }
  case object USDT extends AssetType { val kind = "USDT" }

  val assetTypes: Seq[AssetType] = Seq(BTC, USDT)
  implicit object AssetTypeFormat extends JsonFormat[AssetType] {
    def write(ds: AssetType): JsValue = JsString(ds.kind)

    def read(value: JsValue): AssetType = value match {
      case JsString(kind) => assetTypes.find(_.kind == kind).get
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

  sealed abstract class WithdrawType(val state: Int, val label: String)
  case object Deposit extends WithdrawType(0, "deposit")
  case object Loan extends WithdrawType(2, "interest")
  case object Topup extends WithdrawType(1, "topup")

  val withdrawStates: Seq[WithdrawType] = Seq(Deposit, Topup, Loan)
  implicit object WithdrawTypeFormat extends JsonFormat[WithdrawType] {
    def write(ds: WithdrawType): JsValue = JsNumber(ds.state)

    def read(value: JsValue): WithdrawType = value match {
      case JsNumber(num) => withdrawStates.find(_.state == num).get
      case _ => throw new RuntimeException
    }
  }

  case class WithdrawSource(kind: WithdrawType, id: Long)
  case class WithdrawOption(ws: WithdrawSource, withdrawable: Boolean, amount: BigDecimal, stamp: Long) {
    lazy val msat: MilliSatoshi = Btc(amount.abs).toSatoshi.toMilliSatoshi
  }

  implicit val withdrawSourceFormat: JsonFormat[WithdrawSource] = jsonFormat[WithdrawType, Long, WithdrawSource](WithdrawSource.apply, "kind", "id")
  implicit val withdrawOptionFormat: JsonFormat[WithdrawOption] = jsonFormat[WithdrawSource, Boolean, BigDecimal, Long, WithdrawOption](WithdrawOption.apply, "ws", "withdrawable", "amount", "stamp")

  // Request types

  sealed trait RequestArguments
  case class GetLoanAd(asset: AssetType) extends RequestArguments
  case class GetUserStatus(sessionToken: String) extends RequestArguments
  case class GetWithdrawOptions(asset: AssetType) extends RequestArguments
  case class Login(oneTimePassword: Option[String], email: String) extends RequestArguments
  case class WithdrawReq(asset: AssetType, address: String, ws: List[WithdrawSource] = Nil) extends RequestArguments
  case class DepositIntent(asset: AssetType, txid: String) extends RequestArguments

  implicit val getLoanAddFormat: JsonFormat[GetLoanAd] = taggedJsonFmt(jsonFormat[AssetType, GetLoanAd](GetLoanAd.apply, "asset"), tag = "GetLoanAd")
  implicit val loginFormat: JsonFormat[Login] = taggedJsonFmt(jsonFormat[Option[String], String, Login](Login.apply, "oneTimePassword", "email"), tag = "Login")
  implicit val getUserStatusFormat: JsonFormat[GetUserStatus] = taggedJsonFmt(jsonFormat[String, GetUserStatus](GetUserStatus.apply, "sessionToken"), tag = "GetUserStatus")
  implicit val depositIntentFormat: JsonFormat[DepositIntent] = taggedJsonFmt(jsonFormat[AssetType, String, DepositIntent](DepositIntent.apply, "asset", "txid"), tag = "DepositIntent")
  implicit val withdrawReqFormat: JsonFormat[WithdrawReq] = taggedJsonFmt(jsonFormat[AssetType, String, List[WithdrawSource], WithdrawReq](WithdrawReq.apply, "asset", "address", "ws"), tag = "Withdraw")
  implicit val getWithdrawOptionsFormat: JsonFormat[GetWithdrawOptions] = taggedJsonFmt(jsonFormat[AssetType, GetWithdrawOptions](GetWithdrawOptions.apply, "asset"), tag = "GetWithdrawOptions")

  implicit object RequestArgumentsFormat extends JsonFormat[RequestArguments] {
    def read(json: JsValue): RequestArguments =
      throw new RuntimeException

    def write(obj: RequestArguments): JsValue = obj match {
      case request: WithdrawReq => withdrawReqFormat.write(request)
      case request: GetUserStatus => getUserStatusFormat.write(request)
      case request: DepositIntent => depositIntentFormat.write(request)
      case request: GetWithdrawOptions => getWithdrawOptionsFormat.write(request)
      case request: GetLoanAd => getLoanAddFormat.write(request)
      case request: Login => loginFormat.write(request)
    }
  }

  // Response types

  case class TotalFunds(balance: BigDecimal, withdrawable: BigDecimal)
  case class Deposit(id: Long, txid: String, address: String, amount: BigDecimal, created: Long, state: DepositState)
  case class Withdraw(id: Long, userId: Long, totalAmount: BigDecimal, txid: String, exid: String, address: String,
                      amount: BigDecimal, ws: Option[WithdrawSource], created: Long, started: Boolean, fee: BigDecimal)
  case class ActiveLoan(id: Long, userId: Long, start: Long, end: Long, roi: BigDecimal, amount: BigDecimal) {
    lazy val daysLeft = Math.max(0L, (end - System.currentTimeMillis) / 86400000L)
    lazy val inverseSpan = 31536000000D / (end - start)
    lazy val interest = amount * roi / inverseSpan
  }

  implicit val depositFormat: JsonFormat[Deposit] = jsonFormat[Long, String, String, BigDecimal, Long, DepositState,
    Deposit](Deposit.apply, "id", "txid", "address", "amount", "created", "state")
  implicit val withdrawFormat: JsonFormat[Withdraw] =
    jsonFormat[Long, Long, BigDecimal, String, String, String, BigDecimal, Option[WithdrawSource], Long, Boolean, BigDecimal,
      Withdraw](Withdraw.apply, "id", "userId", "totalAmount", "txid", "exid", "address", "amount", "ws", "created", "started", "fee")
  implicit val activeLoanFormat: JsonFormat[ActiveLoan] = jsonFormat[Long, Long, Long, Long, BigDecimal, BigDecimal,
    ActiveLoan](ActiveLoan.apply, "id", "userId", "start", "end", "roi", "amount")
  implicit val totalFundsFormat: JsonFormat[TotalFunds] = jsonFormat[BigDecimal, BigDecimal,
    TotalFunds](TotalFunds.apply, "balance", "withdrawable")

  case class Request(arguments: RequestArguments, id: String)
  case class Response(arguments: Option[ResponseArguments], id: String)
  case class AssetStatus(asset: AssetType, pendingWithdraws: List[Withdraw], pendingDeposits: List[Deposit], activeLoans: List[ActiveLoan], totalFunds: TotalFunds, withdrawDelay: Long) {
    val withdrawDate = new Date(maxOptionByValue(activeLoans)(_.end, 0L) max maxOptionByValue(pendingWithdraws)(_.created + withdrawDelay, 0L) max System.currentTimeMillis)
  }

  sealed trait ResponseArguments
  case class Failure(failureCode: FailureCode) extends ResponseArguments
  case class WithdrawOptions(asset: AssetType, options: List[WithdrawOption], withdraws: List[Withdraw] = Nil) extends ResponseArguments
  case class LoanAd(asset: AssetType, durationDays: Long, minDeposit: BigDecimal, maxDeposit: BigDecimal, address: String, challenge: String, roi: BigDecimal) extends ResponseArguments with CoinUri {
    lazy val desc: CoinDescription = CoinDescription(addresses = List(address), WalletApp.app.getString(R.string.ta_btc_loan_label).asSome, networkId = -1, taRoi = roi.asSome)
    lazy val maxAmount: MilliSatoshi = Btc(maxDeposit).toSatoshi.toMilliSatoshi
    lazy val amount: Option[MilliSatoshi] = None
  }

  sealed trait TaLinkState
  case object LoggedOut extends TaLinkState
  case class UserStatus(assets: List[AssetStatus], email: String, sessionToken: String) extends ResponseArguments with TaLinkState
  implicit val assetStatusFormat: JsonFormat[AssetStatus] = jsonFormat[AssetType, List[Withdraw], List[Deposit], List[ActiveLoan], TotalFunds, Long,
    AssetStatus](AssetStatus.apply, "asset", "pendingWithdraws", "pendingDeposits", "activeLoans", "totalFunds", "withdrawDelay")

  implicit val failureFormat: JsonFormat[Failure] = jsonFormat[FailureCode, Failure](Failure.apply, "failureCode")
  implicit val loanAdFormat: JsonFormat[LoanAd] = jsonFormat[AssetType, Long, BigDecimal, BigDecimal, String, String, BigDecimal,
    LoanAd](LoanAd.apply, "asset", "durationDays", "minDeposit", "maxDeposit", "address", "challenge", "roi")
  implicit val withdrawOptionsFormat: JsonFormat[WithdrawOptions] = jsonFormat[AssetType, List[WithdrawOption], List[Withdraw],
    WithdrawOptions](WithdrawOptions.apply, "asset", "options", "withdraws")
  implicit val userStatusFormat: JsonFormat[UserStatus] = jsonFormat[List[AssetStatus], String, String,
    UserStatus](UserStatus.apply, "assets", "email", "sessionToken")

  implicit object ResponseArgumentsFormat extends JsonFormat[ResponseArguments] {
    def read(json: JsValue): ResponseArguments = json.asJsObject.fields(TAG) match {
      case JsString("WithdrawOptions") => withdrawOptionsFormat.read(json)
      case JsString("UserStatus") => userStatusFormat.read(json)
      case JsString("Failure") => failureFormat.read(json)
      case JsString("LoanAd") => loanAdFormat.read(json)
      case _ => throw new RuntimeException
    }

    def write(obj: ResponseArguments): JsValue =
      throw new RuntimeException
  }

  implicit val requestFormat: JsonFormat[Request] = jsonFormat[RequestArguments, String, Request](Request.apply, "arguments", "id")
  implicit val responseFormat: JsonFormat[Response] = jsonFormat[Option[ResponseArguments], String, Response](Response.apply, "arguments", "id")

  class Listener(val id: String) {
    def onResponse(args: Option[ResponseArguments] = None): Unit = none
    def onConnected(stateData: TaLinkState): Unit = none
    def onDisconnected: Unit = none
  }

  case class CmdRemove(listener: Listener)
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
    case (req: Request, DISCONNECTED) => listeners.filter(lst => lst.id == req.id || lst.id == ALL_IDS).foreach(_.onDisconnected)
    case (Response(arguments, id), CONNECTED) => listeners.filter(lst => lst.id == id || lst.id == ALL_IDS).foreach(_ onResponse arguments)
    case _ =>
  }

  private val TA_USER_STATUS = "ta-user-status"
  def loadUserStatus: Try[UserStatus] = extDataBag.tryGet(TA_USER_STATUS).map(SQLiteData.byteVecToString) map to[UserStatus]
  def saveUserStatus(newStatus: UserStatus): Unit = extDataBag.put(TA_USER_STATUS, newStatus.toJson.compactPrint getBytes "UTF-8")

  state = DISCONNECTED
  data = LoggedOut
}