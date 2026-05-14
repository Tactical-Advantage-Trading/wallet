package trading.tacticaladvantage.sqlite

import fr.acinq.bitcoin.BlockHeader
import scodec.bits.ByteVector
import spray.json._
import trading.tacticaladvantage.MasterKeys.walletSecretCodec
import trading.tacticaladvantage.Tools._
import trading.tacticaladvantage.WalletSecret
import trading.tacticaladvantage.sqlite.SQLiteData._
import trading.tacticaladvantage.utils.ImplicitJsonFormats._
import trading.tacticaladvantage.utils.{FeeRatesInfo, FiatRatesInfo}

import java.lang.{Integer => JInt}
import scala.util.Try

object SQLiteData {
  final val LABEL_SECRET = "label-secret"
  final val LABEL_BTC_FEE_RATES = "label-btc-fee-rates"
  final val LABEL_ECX_FEE_RATES = "label-ecx-fee-rates"
  final val LABEL_BTC_FIAT_RATES = "label-btc-fiat-rates"
  final val LABEL_ECX_FIAT_RATES = "label-ecx-fiat-rates"

  def byteVecToString(bv: ByteVector): String = new String(bv.toArray, "UTF-8")
  type HeightAndHeader = (Int, BlockHeader)
}

class SQLiteData(val db: DBInterface) {
  def delete(label: String): Unit = db.change(DataTable.killSql, label)

  def tryGet(keyValueLabel: String): Try[ByteVector] =
    db.select(DataTable.selectSql, keyValueLabel)
      .headTry(_ byteVec DataTable.content)

  def put(label: String, content: Bytes): Unit = {
    // Insert and then update because of INSERT IGNORE
    db.change(DataTable.newSql, label, content)
    db.change(DataTable.updSql, content, label)
  }

  // StorageFormat

  def putSecret(secret: WalletSecret): Unit = put(LABEL_SECRET, walletSecretCodec.encode(secret).require.toByteArray)

  def tryGetSecret: Try[WalletSecret] = tryGet(LABEL_SECRET).map(raw => walletSecretCodec.decode(raw.toBitVector).require.value)

  // Fiat rates, fee rates

  def putFiatRatesInfo(data: FiatRatesInfo, label: String): Unit = put(label, data.toJson.compactPrint getBytes "UTF-8")

  def tryGetFiatRatesInfo(label: String): Try[FiatRatesInfo] = tryGet(label).map(SQLiteData.byteVecToString) map to[FiatRatesInfo]

  def putFeeRatesInfo(data: FeeRatesInfo, label: String): Unit = put(label, data.toJson.compactPrint getBytes "UTF-8")

  def tryGetFeeRatesInfo(label: String): Try[FeeRatesInfo] = tryGet(label).map(SQLiteData.byteVecToString) map to[FeeRatesInfo]

  // HeadersDb

  def addHeaders(headers: Seq[BlockHeader], atHeight: Int): Unit = {
    val addHeaderSqlPQ = db.makePreparedQuery(ElectrumHeadersTable.addHeaderSql)

    db txWrap {
      for (header \ idx <- headers.zipWithIndex) {
        val serialized: Array[Byte] = BlockHeader.write(header).toArray
        db.change(addHeaderSqlPQ, atHeight + idx: JInt, header.hash.toHex, serialized)
      }
    }

    addHeaderSqlPQ.close
  }

  def getHeader(height: Int): Option[BlockHeader] =
    db.select(ElectrumHeadersTable.selectByHeightSql, height.toString).headTry { rc =>
      BlockHeader.read(rc bytes ElectrumHeadersTable.header)
    }.toOption

  def getHeaders(startHeight: Int, maxCount: Int): Seq[BlockHeader] =
    db.select(ElectrumHeadersTable.selectHeadersSql, startHeight.toString, maxCount.toString).iterable { rc =>
      BlockHeader.read(rc bytes ElectrumHeadersTable.header)
    }.toList

  def getTip: Option[HeightAndHeader] =
    db.select(ElectrumHeadersTable.selectTipSql).headTry { rc =>
      val header = BlockHeader.read(rc bytes ElectrumHeadersTable.header)
      (rc int ElectrumHeadersTable.height, header)
    }.toOption
}
