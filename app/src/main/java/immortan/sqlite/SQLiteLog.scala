package immortan.sqlite


case class LogRecord(stamp: Long, tag: String, content: String) {
  def asString: String = s"${date.toString} / $tag / $content"
  val date: java.util.Date = new java.util.Date(stamp)
}

class SQLiteLog(db: DBInterface) {
  def count: scala.util.Try[Long] = db.select(LogTable.countSql).headTry(_ long 0)

  def recent: Iterable[LogRecord] = db.select(LogTable.recentSql).iterable(toLog)

  def clear(): Unit = db.change(LogTable.clearSql)

  def put(tag: String, content: String): Unit = db.change(LogTable.newSql, System.currentTimeMillis: java.lang.Long, tag, content)

  def toLog(rc: RichCursor): LogRecord = LogRecord(stamp = rc long LogTable.stamp, tag = rc string LogTable.tag, content = rc string LogTable.content)
}
