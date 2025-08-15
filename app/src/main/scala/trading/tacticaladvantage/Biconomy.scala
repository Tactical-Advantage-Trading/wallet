package trading.tacticaladvantage

import immortan.ConnectionProvider
import immortan.utils.ImplicitJsonFormats._
import spray.json._
import trading.tacticaladvantage.Biconomy._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object Biconomy {
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

class Biconomy(cp: ConnectionProvider, dir: String) {
  @native def startNodeWithArguments(arguments: Array[String] = Array.empty): Integer
  final val endpoint = "http://localhost:3000"
  final val waitUntilOnlineMsec = 2000
  final val maxAttmpts = 10

  var isRunning = false
  def startNode: Unit = Future {
    // We can not start a Node again while it runnig
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

  def getSmartAccountAddress(req: AccountAddressRequest): Option[AccountAddressResponse] = attempt(maxAttmpts) {
    val response = cp.postJson(s"$endpoint/get-smart-account-address", req.toJson.compactPrint)
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
