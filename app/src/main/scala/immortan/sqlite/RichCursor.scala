package immortan.sqlite

import java.sql.ResultSet

import immortan.crypto.Tools.Bytes
import scodec.bits.ByteVector

import scala.util.Try


trait RichCursor extends Iterable[RichCursor] {
  def byteVec(key: String): ByteVector = ByteVector view bytes(key)

  def iterable[T](transform: RichCursor => T): Iterable[T]

  def set[T](transform: RichCursor => T): Set[T]

  def headTry[T](fun: RichCursor => T): Try[T]

  def bytes(key: String): Bytes

  def string(key: String): String

  def long(key: String): Long

  def long(pos: Int): Long

  def int(key: String): Int

  def int(pos: Int): Int
}

case class RichCursorSQLiteGeneral(rs: ResultSet) extends RichCursor { me =>
  def iterable[T](transform: RichCursor => T): Iterable[T] = try map(transform) finally rs.close

  def set[T](transform: RichCursor => T): Set[T] = try map(transform).toSet finally rs.close

  def headTry[T](fun: RichCursor => T): Try[T] = try Try(fun apply head) finally rs.close

  def bytes(key: String): Bytes = rs.getBytes(key)

  def string(key: String): String = rs.getString(key)

  def long(key: String): Long = rs.getLong(key)

  def long(pos: Int): Long = rs.getLong(pos + 1)

  def int(key: String): Int = rs.getInt(key)

  def int(pos: Int): Int = rs.getInt(pos + 1)

  def iterator: Iterator[RichCursor] =
    new Iterator[RichCursor] {
      def hasNext: Boolean = rs.next
      def next: RichCursor = me
    }
}
