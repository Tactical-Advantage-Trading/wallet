package trading.tacticaladvantage.utils

import fr.acinq.bitcoin.{BtcAmount, Satoshi, SatoshiLong}
import fr.acinq.eclair._
import immortan.Tools._
import immortan.utils.Denomination
import immortan.{BtcDescription, PlainBtcDescription}
import trading.tacticaladvantage.utils.InputParser._
import trading.tacticaladvantage.utils.uri.Uri

import scala.util.parsing.combinator.RegexParsers
import scala.util.{Success, Try}

object InputParser {
  var value: Any = new String
  case object DoNotEraseRecordedValue
  type Checker = PartialFunction[Any, Any]

  val bitcoin: String = "bitcoin:"
  val ecash: String = "ecash:"

  def checkAndMaybeErase(fun: Checker): Unit = fun(value) match {
    case DoNotEraseRecordedValue => // Do nothing, value is retained
    case _ => value = null // Erase recorded value
  }

  def removePrefix(raw: String): String = raw.split(':').toList match {
    case prefix :: content if bitcoin.startsWith(prefix.toLowerCase) => content.mkString.replace("//", "")
    case prefix :: content if ecash.startsWith(prefix.toLowerCase) => throw new Exception("TODO Ecash")
    case _ => raw
  }

  def recordValue(raw: String): Unit = value = parse(raw)

  def parse(raw: String): Any = {
    val withoutSlashes = removePrefix(raw take 2880).trim
    val addressToAmount = MultiAddressParser.parseAll(MultiAddressParser.parse, raw)
    addressToAmount getOrElse PlainBitcoinUri.fromRaw(s"$bitcoin$withoutSlashes")
  }
}

trait BitcoinUri {
  val maxAmount: MilliSatoshi
  val amount: Option[MilliSatoshi]
  val desc: BtcDescription
  val address: String
}

object PlainBitcoinUri {
  def fromRaw(raw: String): PlainBitcoinUri = {
    val dataWithoutPrefix = InputParser.removePrefix(raw)
    val uri = Uri.parse(s"$bitcoin//$dataWithoutPrefix")
    PlainBitcoinUri(Success(uri), uri.getHost)
  }
}

case class PlainBitcoinUri(uri: Try[Uri], address: String) extends BitcoinUri {
  val label: Option[String] = uri.map(_ getQueryParameter "label").map(trimmed).filter(_.nonEmpty).toOption
  val amount: Option[MilliSatoshi] = uri.map(_ getQueryParameter "amount").map(BigDecimal.apply).map(Denomination.btcBigDecimal2MSat).toOption
  val desc: BtcDescription = PlainBtcDescription(List(address), label)
  val maxAmount: MilliSatoshi = MAX_MSAT
}

object MultiAddressParser extends RegexParsers {

  type AddressToAmountItem = (String, Satoshi)

  case class AddressToAmount(values: Seq[AddressToAmountItem] = Nil)

  private[this] val longSat = "[0-9,]+".r ^^ (_.replace(",", "").toLong.sat)

  private[this] val decimalSat = "[0-9]*\\.[0-9]+".r ^^ (raw => (BigDecimal(raw) * BtcAmount.Coin).toLong.sat)

  private[this] val item = "\\w+".r ~ (decimalSat | longSat) ^^ { case address ~ sat => address -> sat }

  private[this] val separator = opt(";")

  val parse: Parser[AddressToAmount] = repsep(item, separator).map(AddressToAmount)
}
