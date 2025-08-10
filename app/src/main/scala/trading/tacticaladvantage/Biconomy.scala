package trading.tacticaladvantage

import immortan.ConnectionProvider
import immortan.utils.ImplicitJsonFormats._
import spray.json._
import Biconomy._

object Biconomy {
  System.loadLibrary("native-lib")
  System.loadLibrary("node")

  case class AccountAddressRequest(pk: String)
  case class TxDetailsRequest(pk: String, token: String, recipient: String, amount: String)

  implicit val accountAddressRequestFormat: JsonFormat[AccountAddressRequest] = jsonFormat1(AccountAddressRequest)
  implicit val txDetailsRequestFormat: JsonFormat[TxDetailsRequest] = jsonFormat4(TxDetailsRequest)

  case class AccountAddressResponse(smartAccountAddress: String)
  case class EstimateGasResponse(usdcFeeAmount: String)
  case class OpHashResponse(userOpHash: String)

  implicit val accountAddressResponseFormat: JsonFormat[AccountAddressResponse] = jsonFormat1(AccountAddressResponse)
  implicit val estimateGasResponseFormat: JsonFormat[EstimateGasResponse] = jsonFormat1(EstimateGasResponse)
  implicit val opHashResponseFormat: JsonFormat[OpHashResponse] = jsonFormat1(OpHashResponse)
}

class Biconomy(cp: ConnectionProvider) {
  final val waitUntilNodeOnlineMsec = 2000
  final val endpoint = "http://localhost:3000"
  final val polygonContract = "0xA0b86991c6218b36c1d19D4a2e9Eb0cE3606eB48"
  @native def startNodeWithArguments(arguments: Array[String] = Array.empty): Integer

  // Node takes some time to get the local server up, depending on phone specs
  // To make this smooth for API user we poll a number of times with delay
  def attempt[T](left: Int)(action: => T): Option[T] =
    try Some(action) catch {
      case _: Throwable if left > 0 =>
        Thread.sleep(waitUntilNodeOnlineMsec)
        attempt(left - 1)(action)
      case _ =>
        None
    }

  def getSmartAccountAddress(req: AccountAddressRequest): Option[AccountAddressResponse] = attempt(10) {
    val response = cp.postJson(s"$endpoint/get-smart-account-address", req.toJson.compactPrint)
    to[AccountAddressResponse](response.string)
  }

  def estimateTxGas(req: TxDetailsRequest): Option[EstimateGasResponse] = attempt(10) {
    val response = cp.postJson(s"$endpoint/estimate-gas", req.toJson.compactPrint)
    to[EstimateGasResponse](response.string)
  }

  def send(req: TxDetailsRequest): Option[OpHashResponse] = attempt(10) {
    val response = cp.postJson(s"$endpoint/execute-transaction", req.toJson.compactPrint)
    to[OpHashResponse](response.string)
  }
}
