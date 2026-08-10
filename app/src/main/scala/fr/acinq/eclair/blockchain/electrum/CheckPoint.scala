package fr.acinq.eclair.blockchain.electrum

import fr.acinq.bitcoin.{ByteVector32, encodeCompact}
import org.json4s.JsonAST.{JArray, JInt, JString}
import org.json4s.native.JsonMethods
import trading.tacticaladvantage.sqlite.SQLiteData
import java.io.InputStream


case class CheckPoint(hash: ByteVector32, nextBits: Long)

object CheckPoint {
  import Blockchain.RETARGETING_PERIOD

  def load(stream: InputStream): Vector[CheckPoint] = {
    val JArray(values) = JsonMethods.parse(stream)

    values.collect {
      case JArray(JString(a) :: JInt(b) :: Nil) =>
        val hash = ByteVector32.fromValidHex(a).reverse
        val nextBits = encodeCompact(b.bigInteger)
        CheckPoint(hash, nextBits)
    }.toVector
  }

  def withDbHeaders(checkpoints: Vector[CheckPoint], dataDb: SQLiteData): Vector[CheckPoint] = {
    val start = checkpoints.size * RETARGETING_PERIOD - 1 + RETARGETING_PERIOD

    val newCheckpoints = for {
      (height, _) <- dataDb.getTip.toVector
      height1 <- start to (height - RETARGETING_PERIOD) by RETARGETING_PERIOD
    } yield CheckPoint(dataDb.getHeader(height1).get.hash, dataDb.getHeader(height1 + 1).get.bits)
    checkpoints ++ newCheckpoints
  }
}
