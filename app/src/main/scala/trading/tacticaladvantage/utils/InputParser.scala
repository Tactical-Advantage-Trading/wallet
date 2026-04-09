package trading.tacticaladvantage.utils

import fr.acinq.bitcoin.{BtcAmount, Satoshi, SatoshiLong}
import fr.acinq.eclair._
import fr.acinq.eclair.blockchain.electrum.Electrum
import trading.tacticaladvantage.Tools._
import trading.tacticaladvantage.CoinDescription
import trading.tacticaladvantage.utils.uri.Uri

import scala.util.parsing.combinator.RegexParsers
import scala.util.{Success, Try}


object InputParser {
  var value: Any = new String
  case object DoNotEraseRecordedValue
  type Checker = PartialFunction[Any, Any]

  def recordValue(raw: String): Unit = value = parse(raw)
  def checkAndMaybeErase(fun: Checker): Unit = fun(value) match {
    case DoNotEraseRecordedValue => // Do nothing, value is retained
    case _ => value = null // Erase recorded value
  }

  def removePrefix(raw: String): String =
    raw.split(':').toList match {
      case noPrefixData :: Nil => noPrefixData
      case _ :: data => data.mkString.replace("//", "")
      case Nil => raw
    }

  def parse(raw: String): Any = {
    val withoutSlashes = removePrefix(raw take 2880).trim
    val addressToAmount = MultiAddressParser.parseAll(MultiAddressParser.parse, raw)
    addressToAmount getOrElse PlainCoinUri.fromRaw(s"dummy:$withoutSlashes")
  }
}

trait CoinUri {
  val maxAmount: MilliSatoshi
  val amount: Option[MilliSatoshi]
  val desc: CoinDescription
  val address: String
}

object PlainCoinUri {
  def fromRaw(raw: String): PlainCoinUri = {
    val dataWithoutPrefix = InputParser.removePrefix(raw)
    val uri = Uri.parse(s"dummy://$dataWithoutPrefix")
    PlainCoinUri(Success(uri), uri.getHost)
  }
}

case class PlainCoinUri(uri: Try[Uri], address: String) extends CoinUri {
  def addressGood(electrum: Electrum): Boolean = Try(electrum addressToPubKeyScript address).isSuccess
  val label: Option[String] = uri.map(_ getQueryParameter "label").map(trimmed).filter(_.nonEmpty).toOption
  val amount: Option[MilliSatoshi] = uri.map(_ getQueryParameter "amount").map(BigDecimal.apply).map(Denomination.btcBigDecimal2MSat).toOption
  val desc: CoinDescription = CoinDescription(addresses = List(address), label, networkId = -1)
  val maxAmount: MilliSatoshi = MAX_MSAT
}

object MultiAddressParser extends RegexParsers {

  type AddressToAmountItem = (String, Satoshi)

  case class AddressToAmount(values: Seq[AddressToAmountItem] = Nil) {
    def addressGood(electrum: Electrum): Boolean = values.nonEmpty && values.forall { case (address, amount) =>
      amount > electrum.params.dustLimit && Try(electrum addressToPubKeyScript address).isSuccess
    }
  }

  private[this] val longSat = "[0-9,]+".r ^^ (_.replace(",", "").toLong.sat)

  private[this] val decimalSat = "[0-9]*\\.[0-9]+".r ^^ (raw => (BigDecimal(raw) * BtcAmount.Coin).toLong.sat)

  private[this] val item = "\\w+".r ~ (decimalSat | longSat) ^^ { case address ~ sat => address -> sat }

  private[this] val separator = opt(";")

  val parse: Parser[AddressToAmount] = repsep(item, separator).map(AddressToAmount)
}
