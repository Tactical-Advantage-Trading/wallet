package immortan.sqlite

import java.lang.{Double => JDouble, Integer => JInt, Long => JLong}
import java.sql.PreparedStatement

import immortan.crypto.Tools.Bytes


trait PreparedQuery {
  def bound(params: Object*): PreparedQuery

  def executeQuery: RichCursor

  def executeUpdate: Unit

  def close: Unit
}

case class PreparedQuerySQLiteGeneral(stmt: PreparedStatement) extends PreparedQuery { me =>

  def bound(params: Object*): PreparedQuery = {
    // Mutable, but local and saves one iteration
    var positionIndex = 1

    for (queryParameter <- params) {
      queryParameter match {
        case queryParameter: JDouble => stmt.setDouble(positionIndex, queryParameter)
        case queryParameter: String => stmt.setString(positionIndex, queryParameter)
        case queryParameter: Bytes => stmt.setBytes(positionIndex, queryParameter)
        case queryParameter: JLong => stmt.setLong(positionIndex, queryParameter)
        case queryParameter: JInt => stmt.setInt(positionIndex, queryParameter)
        case _ => throw new RuntimeException
      }

      positionIndex += 1
    }

    me
  }

  def executeQuery: RichCursor = RichCursorSQLiteGeneral(stmt.executeQuery)

  def executeUpdate: Unit = stmt.executeUpdate

  def close: Unit = stmt.close
}