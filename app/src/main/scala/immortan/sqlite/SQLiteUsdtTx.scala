package immortan.sqlite

import java.lang.{Long => JLong}

import fr.acinq.bitcoin.ByteVector32
import immortan.utils.ImplicitJsonFormats._
import immortan.{UsdtDescription, UsdtInfo}
import spray.json._

class SQLiteUsdtTx(val db: DBInterface) {
  def listRecentTxs(limit: Int): RichCursor = db.select(UsdtTxTable.selectRecentSql, limit.toString)
  def searchTransactions(rawSearchQuery: String): RichCursor = db.search(UsdtTxTable.searchSql, rawSearchQuery.toLowerCase)

  def addSearchableTransaction(search: String, hash: ByteVector32): Unit = {
    val newVirtualSqlPQ = db.makePreparedQuery(UsdtTxTable.newVirtualSql)
    db.change(newVirtualSqlPQ, search.toLowerCase, hash.toHex)
    newVirtualSqlPQ.close
  }

  def updDescription(description: UsdtDescription, hash: ByteVector32): Unit = db txWrap {
    val updateDescriptionSqlPQ = db.makePreparedQuery(UsdtTxTable.updateDescriptionSql)
    db.change(updateDescriptionSqlPQ, description.toJson.compactPrint, hash.toHex)
    for (label <- description.label) addSearchableTransaction(label, hash)
    DbStreams.next(DbStreams.txDbStream)
    updateDescriptionSqlPQ.close
  }

  def updStatus(hash: ByteVector32, block: Long, updatedStamp: Long, doubleSpent: Boolean): Unit = {
    db.change(UsdtTxTable.updStatusSql, block: JLong, if (doubleSpent) 1L: JLong else 0L: JLong, updatedStamp: JLong, hash.toHex)
    DbStreams.next(DbStreams.txDbStream)
  }

  def addTx(network: Long, hash: String, block: Long, doubleSpent: Boolean, receivedUsdt: String,
            sentUsdt: String, feeUsdt: String, description: UsdtDescription, isIncoming: Long,
            balanceSnapUsdt: Long, stamp: Long): Unit = {
    val newSqlPQ = db.makePreparedQuery(UsdtTxTable.newSql)
    db.change(newSqlPQ, hash, network: JLong, block: JLong, receivedUsdt, sentUsdt, feeUsdt, stamp: JLong /* SEEN */,
      stamp: JLong /* UPDATED */, description.toJson.compactPrint, balanceSnapUsdt: JLong, isIncoming: JLong,
      if (doubleSpent) 1L: JLong else 0L: JLong /* NOT DOUBLE SPENT YET */)
    DbStreams.next(DbStreams.txDbStream)
    newSqlPQ.close
  }

  def toUsdtInfo(rc: RichCursor): UsdtInfo = {
    UsdtInfo(hashString = rc string UsdtTxTable.hash, network = rc int UsdtTxTable.network, block = rc long UsdtTxTable.block,
      receivedUsdtString = rc string UsdtTxTable.receivedUsdt, sentUsdtString = rc string UsdtTxTable.sentUsdt, feeUsdtString = rc string UsdtTxTable.feeUsdt,
      seenAt = rc long UsdtTxTable.seenAt, updatedAt = rc long UsdtTxTable.updatedAt, description = to[UsdtDescription](rc string UsdtTxTable.description),
      balanceUsdt = rc long UsdtTxTable.balanceUsdt, incoming = rc long UsdtTxTable.incoming, doubleSpent = rc long UsdtTxTable.doubleSpent)
  }
}
