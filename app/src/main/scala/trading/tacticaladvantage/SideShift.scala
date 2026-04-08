package trading.tacticaladvantage

import immortan.ConnectionProvider
import immortan.utils.ImplicitJsonFormats._
import spray.json._

import scala.util.Try

object SideShift {
  case class QuoteResponse(id: String, createdAt: String, depositCoin: String, settleCoin: String,
                           depositNetwork: String, settleNetwork: String, expiresAt: String,
                           depositAmount: String, settleAmount: String, rate: String)

  case class ShiftResponse(id: String, createdAt: String, depositCoin: String, settleCoin: String, depositNetwork: String,
                           settleNetwork: String, depositAddress: String, settleAddress: String, depositMin: String, depositMax: String,
                           quoteId: String, depositAmount: String, settleAmount: String, expiresAt: String, status: String, rate: String)

  implicit val quoteResponseFormat: JsonFormat[QuoteResponse] =
    jsonFormat[String, String, String, String, String, String, String, String, String, String,
      QuoteResponse](QuoteResponse.apply, "id", "createdAt", "depositCoin", "settleCoin", "depositNetwork",
      "settleNetwork", "expiresAt", "depositAmount", "settleAmount", "rate")

  implicit val shiftResponseFormat: JsonFormat[ShiftResponse] =
    jsonFormat[String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String,
      ShiftResponse](ShiftResponse.apply, "id", "createdAt", "depositCoin", "settleCoin", "depositNetwork", "settleNetwork", "depositAddress",
      "settleAddress", "depositMin", "depositMax", "quoteId", "depositAmount", "settleAmount", "expiresAt", "status", "rate")

  val baseUrl = s"https://tactical-advantage.trading:8080/shift"

  def quote(cp: ConnectionProvider, depositCoin: String, settleCoin: String, depositAmount: String): Try[QuoteResponse] =
    tryTo[QuoteResponse](cp.get(s"$baseUrl/quote?depositcoin=$depositCoin&settlecoin=$settleCoin&depositamount=$depositAmount").string)

  def fixed(cp: ConnectionProvider, settleAddress: String, refundAddress: String, quoteId: String): Try[ShiftResponse] =
    tryTo[ShiftResponse](cp.get(s"$baseUrl/fixed?settleaddress=$settleAddress&refundaddress=$refundAddress&quoteid=$quoteId").string)

  def status(cp: ConnectionProvider, shiftId: String): Try[ShiftResponse] =
    tryTo[ShiftResponse](cp.get(s"$baseUrl/status?shiftid=$shiftId").string)
}
