package trading.tacticaladvantage.sqlite

import android.content.Context
import android.database.sqlite.{SQLiteDatabase, SQLiteOpenHelper}
import immortan.sqlite.{TxTable, WalletTable, DBInterface, DataTable, ElectrumHeadersTable, PreparedQuery, RichCursor}

class DBInterfaceSQLiteAndroid(context: Context, name: String) extends SQLiteOpenHelper(context, name, null, 3) with DBInterface {
  val base: SQLiteDatabase = getWritableDatabase

  def change(sql: String, params: Object*): Unit = base.execSQL(sql, params.toArray)

  def change(prepared: PreparedQuery, params: Object*): Unit = prepared.bound(params:_*).executeUpdate

  def select(prepared: PreparedQuery, params: String*): RichCursor = throw new RuntimeException("Not supported")

  def select(sql: String, params: String*): RichCursor = {
    val cursor = base.rawQuery(sql, params.toArray)
    RichCursorSQLiteAndroid(cursor)
  }

  def makePreparedQuery(sql: String): PreparedQuery = PreparedQuerySQLiteAndroid(base compileStatement sql)

  def txWrap[T](run: => T): T =
    try {
      base.beginTransaction
      val executionResult = run
      base.setTransactionSuccessful
      executionResult
    } finally {
      base.endTransaction
    }

  def onCreate(dbs: SQLiteDatabase): Unit = {
    TxTable.createStatements.foreach(dbs.execSQL)
    WalletTable.createStatements.foreach(dbs.execSQL)
    ElectrumHeadersTable.createStatements.foreach(dbs.execSQL)
    DataTable.createStatements.foreach(dbs.execSQL)
  }

  def onUpgrade(dbs: SQLiteDatabase, v0: Int, v1: Int): Unit = {
    // Do nothing for now
  }
}
