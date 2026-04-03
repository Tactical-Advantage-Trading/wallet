package fr.acinq.eclair.blockchain.electrum

import fr.acinq.bitcoin.{ByteVector32, encodeCompact}
import immortan.sqlite.SQLiteData
import org.json4s.JsonAST.{JArray, JInt, JString}
import org.json4s.native.JsonMethods
import java.io.InputStream


case class CheckPoint(hash: ByteVector32, nextBits: Long)

object CheckPoint {
  import Blockchain.RETARGETING_PERIOD

  def load(stream: InputStream, headerDb: SQLiteData): Vector[CheckPoint] = {
    val JArray(values) = JsonMethods.parse(stream)

    val checkpoints = values.collect {
      case JArray(JString(a) :: JInt(b) :: Nil) =>
        val hash = ByteVector32.fromValidHex(a).reverse
        val nextBits = encodeCompact(b.bigInteger)
        CheckPoint(hash, nextBits)
    }.toVector

    headerDb.getTip.map { case (height, _) =>
      val newCheckpoints = for {
        height1 <- checkpoints.size * RETARGETING_PERIOD - 1 + RETARGETING_PERIOD to height - RETARGETING_PERIOD by RETARGETING_PERIOD
      } yield CheckPoint(headerDb.getHeader(height1).get.hash, headerDb.getHeader(height1 + 1).get.bits)
      checkpoints ++ newCheckpoints
    } getOrElse checkpoints
  }
}
