package trading.tacticaladvantage.utils

import fr.acinq.bitcoin.{BtcAmount, Satoshi, SatoshiLong}
import fr.acinq.eclair._
import immortan.Tools._
import immortan.utils.Denomination
import trading.tacticaladvantage.utils.uri.Uri
import org.bouncycastle.util.encoders.Base64
import scodec.bits.ByteVector

import scala.util.parsing.combinator.RegexParsers
import scala.util.{Success, Try}
import InputParser._


object InputParser {
  var value: Any = new String
  case object DoNotEraseRecordedValue
  type Checker = PartialFunction[Any, Any]

  def checkAndMaybeErase(fun: Checker): Unit = fun(value) match {
    case DoNotEraseRecordedValue => // Do nothing, value is retained
    case _ => value = null // Erase recorded value
  }

  def removePrefix(raw: String): String = raw.split(':').toList match {
    case prefix :: content if bitcoin.startsWith(prefix.toLowerCase) =>
      content.mkString.replace("//", "")
    case _ => raw
  }

  def recordValue(raw: String): Unit = value = parse(raw)

  private[this] val bip322Sign = "(?im).*?(bip322sign)([A-Za-z0-9+/=a-fA-F|-]+)".r.unanchored
  private[this] val bip322Verify = "(?im).*?(bip322verify)([A-Za-z0-9+/=a-fA-F|-]+)".r.unanchored
  val bitcoin: String = "bitcoin:"

  def parse(rawInput: String): Any = rawInput take 2880 match {
    case bip322Sign(_, rawData) => BIP322Data.parseSign(rawData)
    case bip322Verify(_, rawData) => BIP322Data.parseVerify(rawData)
    case _ if rawInput.startsWith("0x") => rawInput

    case _ =>
      val withoutSlashes = removePrefix(rawInput).trim
      val addressToAmount = MultiAddressParser.parseAll(MultiAddressParser.parse, rawInput)
      addressToAmount getOrElse BitcoinUri.fromRaw(s"$bitcoin$withoutSlashes")
  }
}

object BitcoinUri {
  def fromRaw(raw: String): BitcoinUri = {
    val dataWithoutPrefix = InputParser.removePrefix(raw)
    val uri = Uri.parse(s"$bitcoin//$dataWithoutPrefix")
    BitcoinUri(Success(uri), uri.getHost)
  }
}

case class BitcoinUri(uri: Try[Uri], address: String) {
  val amount: Option[MilliSatoshi] = uri.map(_ getQueryParameter "amount").map(BigDecimal.apply).map(Denomination.btcBigDecimal2MSat).toOption
  val message: Option[String] = uri.map(_ getQueryParameter "message").map(trimmed).filter(_.nonEmpty).toOption
  val label: Option[String] = uri.map(_ getQueryParameter "label").map(trimmed).filter(_.nonEmpty).toOption
}

object BIP322Data {
  def fromBase64String(raw64: String) = new String(Base64 decode raw64)
  def toBase64String(raw: String) = Base64.toBase64String(raw.getBytes)

  def parseVerify(raw: String): BIP322VerifyData = raw split '|' match {
    case Array(address, hash, sig64, "-") => BIP322VerifyData(address, ByteVector.fromValidHex(hash), sig64, Option.empty)
    case Array(address, hash, sig64, msg) => BIP322VerifyData(address, ByteVector.fromValidHex(hash), sig64, fromBase64String(msg).asSome)
    case _ => throw new RuntimeException
  }

  def parseSign(raw: String): BIP32SignData = raw split '|' match {
    case Array(message, address) => BIP32SignData(message, address)
    case _ => throw new RuntimeException
  }
}

case class BIP322VerifyData(address: String, messageHash: ByteVector, signature64: String, message: Option[String] = None) {
  def serialize: String = s"bip322verify$address|${messageHash.toHex}|$signature64|${message.map(_.getBytes).map(Base64.toBase64String) getOrElse "-"}"
}

case class BIP32SignData(message64: String, address: String) {
  lazy val message: String = BIP322Data.fromBase64String(message64)
  def serialize: String = s"bip322sign$message64|$address"
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
