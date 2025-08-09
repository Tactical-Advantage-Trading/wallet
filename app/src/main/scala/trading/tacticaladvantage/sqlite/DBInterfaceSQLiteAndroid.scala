package trading.tacticaladvantage.sqlite

import android.database.sqlite.SQLiteDatabase
import immortan.sqlite.{DBInterface, PreparedQuery, RichCursor}


trait DBInterfaceSQLiteAndroid extends DBInterface {
  def change(sql: String, params: Object*): Unit = base.execSQL(sql, params.toArray)

  def change(prepared: PreparedQuery, params: Object*): Unit = prepared.bound(params:_*).executeUpdate

  def select(prepared: PreparedQuery, params: String*): RichCursor = throw new RuntimeException("Not supported")

  def select(sql: String, params: String*): RichCursor = {
    val cursor = base.rawQuery(sql, params.toArray)
    RichCursorSQLiteAndroid(cursor)
  }

  def makePreparedQuery(sql: String): PreparedQuery =
    PreparedQuerySQLiteAndroid(base compileStatement sql)

  def txWrap[T](run: => T): T =
    try {
      base.beginTransaction
      val executionResult = run
      base.setTransactionSuccessful
      executionResult
    } finally {
      base.endTransaction
    }

  val base: SQLiteDatabase
}
