package trading.tacticaladvantage

import immortan.ConnectionProvider
import immortan.utils.ImplicitJsonFormats._
import spray.json._
import Biconomy._

object Biconomy {
  trait Request
  case class AccountAddressRequest(pk: String) extends Request
  case class TxDetailsRequest(pk: String, token: String, recipient: String, amount: String) extends Request

  implicit val accountAddressRequestFormat: JsonFormat[AccountAddressRequest] = jsonFormat1(AccountAddressRequest)
  implicit val txDetailsRequestFormat: JsonFormat[TxDetailsRequest] = jsonFormat4(TxDetailsRequest)

  trait Response
  case class AccountAddressResponse(smartAccountAddress: String) extends Response
  case class EstimateGasResponse(usdcFeeAmount: String) extends Response
  case class OpHashResponse(userOpHash: String) extends Response

  implicit val accountAddressResponseFormat: JsonFormat[AccountAddressResponse] = jsonFormat1(AccountAddressResponse)
  implicit val estimateGasResponseFormat: JsonFormat[EstimateGasResponse] = jsonFormat1(EstimateGasResponse)
  implicit val opHashResponseFormat: JsonFormat[OpHashResponse] = jsonFormat1(OpHashResponse)
}

class Biconomy(cp: ConnectionProvider) {
  final val waitUntilNodeOnlineMsec = 2000
  final val endpoint = "http://localhost:3000"
  final val polygonContract = "0xA0b86991c6218b36c1d19D4a2e9Eb0cE3606eB48"
  @native def startNodeWithArguments(arguments: Array[String] = Array.empty): Integer
  System.loadLibrary("native-lib")
  System.loadLibrary("node")

  def attempt[T](left: Int)(action: => T): T =
    try action catch {
      case _: Throwable if left > 0 =>
        Thread.sleep(waitUntilNodeOnlineMsec)
        attempt(left - 1)(action)
      case exception =>
        throw exception
    }

  def getSmartAccountAddress(req: AccountAddressRequest): AccountAddressResponse = attempt(10) {
    val response = cp.postJson(s"$endpoint/get-smart-account-address", req.toJson.compactPrint)
    to[AccountAddressResponse](response.string)
  }

  def estimateTxGas(req: TxDetailsRequest): EstimateGasResponse = attempt(10) {
    val response = cp.postJson(s"$endpoint/estimate-gas", req.toJson.compactPrint)
    to[EstimateGasResponse](response.string)
  }

  def send(req: TxDetailsRequest): OpHashResponse = attempt(10) {
    val response = cp.postJson(s"$endpoint/execute-transaction", req.toJson.compactPrint)
    to[OpHashResponse](response.string)
  }
}
