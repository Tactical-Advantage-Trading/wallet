package trading.tacticaladvantage.sqlite

import android.content.Context
import android.database.sqlite._
import immortan.sqlite._


class DBInterfaceSQLiteAndroidMisc(context: Context, name: String) extends SQLiteOpenHelper(context, name, null, 3) with DBInterfaceSQLiteAndroid {
  val base: SQLiteDatabase = getWritableDatabase

  def onCreate(dbs: SQLiteDatabase): Unit = {
    TxTable.createStatements.foreach(dbs.execSQL)
    ElectrumHeadersTable.createStatements.foreach(dbs.execSQL)
    ChainWalletTable.createStatements.foreach(dbs.execSQL)
    DataTable.createStatements.foreach(dbs.execSQL)
  }

  def onUpgrade(dbs: SQLiteDatabase, v0: Int, v1: Int): Unit = {
    // Do nothing for now
  }
}