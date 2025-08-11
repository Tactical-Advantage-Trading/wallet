package immortan.sqlite

import java.lang.{Integer => JInt}

import fr.acinq.bitcoin.{BlockHeader, ByteVector32}
import fr.acinq.eclair.blockchain.electrum.db.HeaderDb
import immortan.WalletSecret
import immortan.crypto.Tools.Bytes
import immortan.sqlite.SQLiteData._
import immortan.utils.ImplicitJsonFormats._
import immortan.utils.{FeeRatesInfo, FiatRatesInfo}
import immortan.MasterKeys.walletSecretCodec
import scodec.bits.ByteVector
import spray.json._

import scala.util.Try


object SQLiteData {
  final val LABEL_FORMAT = "label-format"
  final val LABEL_FEE_RATES = "label-fee-rates"
  final val LABEL_FIAT_RATES = "label-fiat-rates"
  def byteVecToString(bv: ByteVector): String = new String(bv.toArray, "UTF-8")
}

class SQLiteData(val db: DBInterface) extends HeaderDb {
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

  def putSecret(secret: WalletSecret): Unit = put(LABEL_FORMAT, walletSecretCodec.encode(secret).require.toByteArray)

  def tryGetSecret: Try[WalletSecret] = tryGet(LABEL_FORMAT).map(raw => walletSecretCodec.decode(raw.toBitVector).require.value)

  // Fiat rates, fee rates

  def putFiatRatesInfo(data: FiatRatesInfo): Unit = put(LABEL_FIAT_RATES, data.toJson.compactPrint getBytes "UTF-8")

  def tryGetFiatRatesInfo: Try[FiatRatesInfo] = tryGet(LABEL_FIAT_RATES).map(SQLiteData.byteVecToString) map to[FiatRatesInfo]

  def putFeeRatesInfo(data: FeeRatesInfo): Unit = put(LABEL_FEE_RATES, data.toJson.compactPrint getBytes "UTF-8")

  def tryGetFeeRatesInfo: Try[FeeRatesInfo] = tryGet(LABEL_FEE_RATES).map(SQLiteData.byteVecToString) map to[FeeRatesInfo]

  // HeadersDb

  override def addHeaders(headers: Seq[BlockHeader], atHeight: Int): Unit = {
    val addHeaderSqlPQ = db.makePreparedQuery(ElectrumHeadersTable.addHeaderSql)

    db txWrap {
      for (Tuple2(header, idx) <- headers.zipWithIndex) {
        val serialized: Array[Byte] = BlockHeader.write(header).toArray
        db.change(addHeaderSqlPQ, atHeight + idx: JInt, header.hash.toHex, serialized)
      }
    }

    addHeaderSqlPQ.close
  }

  override def getHeader(height: Int): Option[BlockHeader] =
    db.select(ElectrumHeadersTable.selectByHeightSql, height.toString).headTry { rc =>
      BlockHeader.read(rc bytes ElectrumHeadersTable.header)
    }.toOption

  // Only used in testing currently
  override def getHeader(blockHash: ByteVector32): Option[HeightAndHeader] =
    db.select(ElectrumHeadersTable.selectByBlockHashSql, blockHash.toHex).headTry { rc =>
      val header = BlockHeader.read(rc bytes ElectrumHeadersTable.header)
      val height = rc int ElectrumHeadersTable.height
      (height, header)
    }.toOption

  override def getHeaders(startHeight: Int, maxCount: Int): Seq[BlockHeader] =
    db.select(ElectrumHeadersTable.selectHeadersSql, startHeight.toString, maxCount.toString).iterable { rc =>
      BlockHeader.read(rc bytes ElectrumHeadersTable.header)
    }.toList

  override def getTip: Option[HeightAndHeader] =
    db.select(ElectrumHeadersTable.selectTipSql).headTry { rc =>
      val header = BlockHeader.read(rc bytes ElectrumHeadersTable.header)
      val height = rc int ElectrumHeadersTable.height
      (height, header)
    }.toOption
}
