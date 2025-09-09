package immortan.utils

import fr.acinq.bitcoin.Crypto.PublicKey
import fr.acinq.bitcoin.DeterministicWallet.{ExtendedPrivateKey, ExtendedPublicKey}
import fr.acinq.bitcoin.{ByteVector32, Satoshi}
import fr.acinq.eclair.MilliSatoshi
import fr.acinq.eclair.blockchain.fee._
import fr.acinq.eclair.wire.CommonCodecs._
import immortan._
import Tools.{Fiat2Btc, StringList}
import immortan.sqlite.SigningWallet
import immortan.utils.FiatRates.CoinGeckoItemMap
import scodec.bits.BitVector
import spray.json._

import scala.util.Try

object ImplicitJsonFormats extends DefaultJsonProtocol {
  val json2String: JsValue => String = value => value.convertTo[String]
  def json2BitVec(json: JsValue): Option[BitVector] = BitVector fromHex json2String(json)

  final val TAG = "tag"

  def writeExt(ext: (String, JsValue), base: JsValue): JsObject = JsObject(base.asJsObject.fields + ext)

  def to[T : JsonFormat](raw: String): T = raw.parseJson.convertTo[T]

  def tryTo[T: JsonFormat](raw: String): Try[T] = Try {
    to[T](raw)
  }

  def taggedJsonFmt[T](base: JsonFormat[T], tag: String): JsonFormat[T] = new JsonFormat[T] {
    def write(unserialized: T): JsValue = writeExt(TAG -> JsString(tag), base write unserialized)
    def read(serialized: JsValue): T = base read serialized
  }

  def sCodecJsonFmt[T](codec: scodec.Codec[T] = null): JsonFormat[T] = new JsonFormat[T] {
    def read(serialized: JsValue): T = codec.decode(json2BitVec(serialized).get).require.value
    def write(unserialized: T): JsValue = codec.encode(unserialized).require.toHex.toJson
  }

  implicit val publicKeyFmt: JsonFormat[PublicKey] = sCodecJsonFmt(publicKey)

  implicit val byteVector32Fmt: JsonFormat[ByteVector32] = sCodecJsonFmt(bytes32)

  implicit val milliSatoshiFmt: JsonFormat[MilliSatoshi] = jsonFormat[Long, MilliSatoshi](MilliSatoshi.apply, "underlying")

  implicit val satoshiFmt: JsonFormat[Satoshi] = jsonFormat[Long, Satoshi](Satoshi.apply, "underlying")

  implicit val extendedPublicKeyFmt: JsonFormat[ExtendedPublicKey] = sCodecJsonFmt(extendedPublicKeyCodec)

  implicit val extendedPrivateKeyFmt: JsonFormat[ExtendedPrivateKey] = sCodecJsonFmt(extendedPrivateKeyCodec)

  // BTC description

  implicit val signingWalletFmt: JsonFormat[SigningWallet] = taggedJsonFmt(jsonFormat[String, Option[ExtendedPrivateKey], Option[Long],
    SigningWallet](SigningWallet.apply, "walletType", "attachedMaster", "masterFingerprint"), tag = "SigningWallet")

  implicit val semanticOrderFmt: JsonFormat[SemanticOrder] = jsonFormat[String, Long, SemanticOrder](SemanticOrder.apply, "id", "order")
  implicit val rbfParams: JsonFormat[RBFParams] = jsonFormat[ByteVector32, Long, RBFParams](RBFParams.apply, "ofTxid", "mode")

  implicit object BtcDescriptionFmt extends JsonFormat[BtcDescription] {
    def read(raw: JsValue): BtcDescription = raw.asJsObject.fields(TAG) match {
      case JsString("PlainBtcDescription") => raw.convertTo[PlainBtcDescription]
      case _ => throw new Exception
    }

    def write(internal: BtcDescription): JsValue = internal match {
      case btcDescription: PlainBtcDescription => btcDescription.toJson
      case _ => throw new Exception
    }
  }

  implicit val plainBtcDescriptionFmt: JsonFormat[PlainBtcDescription] =
    taggedJsonFmt(jsonFormat[StringList, Option[String], Option[SemanticOrder], Option[ByteVector32], Option[ByteVector32], Option[RBFParams], Option[BigDecimal],
      PlainBtcDescription](PlainBtcDescription.apply, "addresses", "label", "semanticOrder", "cpfpBy", "cpfpOf", "rbf", "taRoi"), tag = "PlainBtcDescription")

  // USDT description

  implicit val usdtDescriptionFmt: JsonFormat[UsdtDescription] = taggedJsonFmt(jsonFormat[String, String, Option[String], Option[BigDecimal],
    UsdtDescription](UsdtDescription.apply, "fromAddrString", "toAddrString", "label", "taRoi"), tag = "UsdtDescription")

  // Fiat feerates

  implicit val blockchainInfoItemFmt: JsonFormat[BlockchainInfoItem] = jsonFormat[Double, BlockchainInfoItem](BlockchainInfoItem.apply, "last")

  implicit val coinGeckoItemFmt: JsonFormat[CoinGeckoItem] = jsonFormat[Double, CoinGeckoItem](CoinGeckoItem.apply, "value")

  implicit val coinGeckoFmt: JsonFormat[CoinGecko] = jsonFormat[CoinGeckoItemMap, CoinGecko](CoinGecko.apply, "rates")

  implicit val fiatRatesInfoFmt: JsonFormat[FiatRatesInfo] = jsonFormat[Fiat2Btc, Fiat2Btc, Long, FiatRatesInfo](FiatRatesInfo.apply, "rates", "oldRates", "stamp")

  // Chain feerates

  implicit val bitGoFeeRateStructureFmt: JsonFormat[BitGoFeeRateStructure] = jsonFormat[Map[String, Long], Long, BitGoFeeRateStructure](BitGoFeeRateStructure.apply, "feeByBlockTarget", "feePerKb")

  implicit val feeratePerKBFmt: JsonFormat[FeeratePerKB] = jsonFormat[Satoshi, FeeratePerKB](FeeratePerKB.apply, "feerate")

  implicit val feeratesPerKBFmt: JsonFormat[FeeratesPerKB] =
    jsonFormat[FeeratePerKB, FeeratePerKB, FeeratePerKB, FeeratePerKB, FeeratePerKB, FeeratePerKB, FeeratePerKB, FeeratePerKB, FeeratePerKB,
      FeeratesPerKB](FeeratesPerKB.apply, "mempoolMinFee", "block_1", "blocks_2", "blocks_6", "blocks_12", "blocks_36", "blocks_72", "blocks_144", "blocks_1008")

  implicit val feeratePerKwFmt: JsonFormat[FeeratePerKw] = jsonFormat[Satoshi, FeeratePerKw](FeeratePerKw.apply, "feerate")

  implicit val feeratesPerKwFmt: JsonFormat[FeeratesPerKw] =
    jsonFormat[FeeratePerKw, FeeratePerKw, FeeratePerKw, FeeratePerKw, FeeratePerKw, FeeratePerKw, FeeratePerKw, FeeratePerKw, FeeratePerKw,
      FeeratesPerKw](FeeratesPerKw.apply, "mempoolMinFee", "block_1", "blocks_2", "blocks_6", "blocks_12", "blocks_36", "blocks_72", "blocks_144", "blocks_1008")

  implicit val feeRatesInfoFmt: JsonFormat[FeeRatesInfo] = jsonFormat[FeeratesPerKw, List[FeeratesPerKB], Long, FeeRatesInfo](FeeRatesInfo.apply, "smoothed", "history", "stamp")
}
