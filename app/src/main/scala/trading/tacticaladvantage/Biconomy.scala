package trading.tacticaladvantage

import spray.json._
import trading.tacticaladvantage.Biconomy._
import immortan.utils.ImplicitJsonFormats._
import scala.concurrent.ExecutionContext.Implicits.global
import immortan.ConnectionProvider
import scala.concurrent.Future

object Biconomy {
  var isRunning: Boolean = false
  final val USDt = "0xc2132D05D31c914a87C6611C10748AEb04B58e8F"

  case class AccountAddressRequest(pk: String)
  case class TxDetailsRequest(pk: String, recipient: String, token: String, amount: String = "1000")

  implicit val accountAddressRequestFormat: JsonFormat[AccountAddressRequest] = jsonFormat[String, AccountAddressRequest](AccountAddressRequest.apply, "pk")
  implicit val txDetailsRequestFormat: JsonFormat[TxDetailsRequest] = jsonFormat[String, String, String, String, TxDetailsRequest](TxDetailsRequest.apply, "pk", "recipient", "token", "amount")

  case class AccountAddressResponse(smartAccountAddress: String)
  case class EstimateGasResponse(feeAmount: String)
  case class OpHashResponse(userOpHash: String)

  implicit val accountAddressResponseFormat: JsonFormat[AccountAddressResponse] = jsonFormat[String, AccountAddressResponse](AccountAddressResponse.apply, "smartAccountAddress")
  implicit val estimateGasResponseFormat: JsonFormat[EstimateGasResponse] = jsonFormat[String, EstimateGasResponse](EstimateGasResponse.apply, "feeAmount")
  implicit val opHashResponseFormat: JsonFormat[OpHashResponse] = jsonFormat[String, OpHashResponse](OpHashResponse.apply, "userOpHash")
}

class Biconomy(cp: ConnectionProvider, dir: String) {
  final val endpoint = "http://localhost:3000"
  final val waitUntilOnlineMsec = 2000
  final val maxAttmpts = 10

  @native
  def startNodeWithArguments(arguments: Array[String] = Array.empty): Integer

  def startNode: Unit = Future {
    // We can't start a Node again while it's runnig
    if (isRunning) return else isRunning = true

    // Safe to load multiple times
    System.loadLibrary("native-lib")
    System.loadLibrary("node")

    // Snap back to not running if node gets killed
    val args = Array("node", s"$dir/server.js")
    startNodeWithArguments(args)
    isRunning = false
  }

  // Node takes some time to get the local server up, depending on phone specs
  // To make this smooth for API user we poll a number of times with delay
  def attempt[T](left: Int)(action: => T): Option[T] =
    try Some(action) catch {
      case _: Throwable if left > 0 =>
        if (left == maxAttmpts) startNode
        Thread.sleep(waitUntilOnlineMsec)
        attempt(left - 1)(action)
      case _: Throwable =>
        None
    }

  def getSmartAccountAddress(privKey: String): Option[AccountAddressResponse] = attempt(maxAttmpts) {
    val response = cp.postJson(s"$endpoint/get-smart-account-address", AccountAddressRequest(privKey).toJson.compactPrint)
    to[AccountAddressResponse](response.string)
  }

  def estimateTxGas(req: TxDetailsRequest): Option[EstimateGasResponse] = attempt(maxAttmpts) {
    val response = cp.postJson(s"$endpoint/estimate-gas", req.toJson.compactPrint)
    to[EstimateGasResponse](response.string)
  }

  def send(req: TxDetailsRequest): Option[OpHashResponse] = attempt(maxAttmpts) {
    val response = cp.postJson(s"$endpoint/execute-transaction", req.toJson.compactPrint)
    to[OpHashResponse](response.string)
  }
}
