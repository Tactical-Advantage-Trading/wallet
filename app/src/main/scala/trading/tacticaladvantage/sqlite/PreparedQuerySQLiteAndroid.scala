package trading.tacticaladvantage.sqlite

import java.lang.{Double => JDouble, Integer => JInt, Long => JLong}

import android.database.sqlite.SQLiteStatement
import immortan.crypto.Tools.Bytes
import immortan.sqlite.{PreparedQuery, RichCursor}


case class PreparedQuerySQLiteAndroid(prepared: SQLiteStatement) extends PreparedQuery { me =>

  def bound(params: Object*): PreparedQuery = {
    // Mutable, but local and saves one iteration
    var positionIndex = 1

    for (queryParameter <- params) {
      queryParameter match {
        case queryParameter: JInt => prepared.bindLong(positionIndex, queryParameter.toLong)
        case queryParameter: JDouble => prepared.bindDouble(positionIndex, queryParameter)
        case queryParameter: String => prepared.bindString(positionIndex, queryParameter)
        case queryParameter: Bytes => prepared.bindBlob(positionIndex, queryParameter)
        case queryParameter: JLong => prepared.bindLong(positionIndex, queryParameter)
        case _ => throw new RuntimeException
      }

      positionIndex += 1
    }

    me
  }

  def executeQuery: RichCursor = throw new RuntimeException("Not supported")

  def executeUpdate: Unit = prepared.executeUpdateDelete

  def close: Unit = prepared.close
}
