package trading.tacticaladvantage.sqlite

import android.database.Cursor
import immortan.Tools.{Bytes, runAnd}
import immortan.sqlite.RichCursor

import scala.util.Try


case class RichCursorSQLiteAndroid(c: Cursor) extends RichCursor { me =>
  def iterable[T](transform: RichCursor => T): Iterable[T] = try map(transform) finally c.close

  def set[T](transform: RichCursor => T): Set[T] = try map(transform).toSet finally c.close

  def headTry[T](fun: RichCursor => T): Try[T] = try Try(fun apply head) finally c.close

  def bytes(key: String): Bytes = c.getBlob(c getColumnIndex key)

  def string(key: String): String = c.getString(c getColumnIndex key)

  def long(key: String): Long = c.getLong(c getColumnIndex key)

  @inline def long(pos: Int): Long = c.getLong(pos)

  def int(key: String): Int = c.getInt(c getColumnIndex key)

  @inline def int(key: Int): Int = c.getInt(key)

  private val resultCount = c.getCount

  def iterator: Iterator[RichCursor] =
    new Iterator[RichCursor] {
      def hasNext: Boolean = c.getPosition < resultCount - 1
      def next: RichCursor = runAnd(me)(c.moveToNext)
    }
}
