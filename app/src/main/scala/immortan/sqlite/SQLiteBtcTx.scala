package immortan.sqlite

import java.lang.{Long => JLong}

import fr.acinq.bitcoin.DeterministicWallet.ExtendedPublicKey
import fr.acinq.bitcoin.{ByteVector32, Satoshi, Transaction}
import fr.acinq.eclair.MilliSatoshi
import immortan.Tools.Fiat2Btc
import immortan.utils.ImplicitJsonFormats._
import immortan.{BtcDescription, BtcInfo}
import spray.json._


class SQLiteBtcTx(val db: DBInterface) {
  def listRecentTxs(limit: Int): RichCursor = db.select(BtcTxTable.selectRecentSql, limit.toString)
  def searchTransactions(rawSearchQuery: String): RichCursor = db.search(BtcTxTable.searchSql, rawSearchQuery.toLowerCase)

  def addSearchableTransaction(search: String, txid: ByteVector32): Unit = {
    val newVirtualSqlPQ = db.makePreparedQuery(BtcTxTable.newVirtualSql)
    db.change(newVirtualSqlPQ, search.toLowerCase, txid.toHex)
    newVirtualSqlPQ.close
  }

  def updDescription(description: BtcDescription, txid: ByteVector32): Unit = db txWrap {
    val updateDescriptionSqlPQ = db.makePreparedQuery(BtcTxTable.updateDescriptionSql)
    db.change(updateDescriptionSqlPQ, description.toJson.compactPrint, txid.toHex)
    for (label <- description.label) addSearchableTransaction(label, txid)
    DbStreams.next(DbStreams.txDbStream)
    updateDescriptionSqlPQ.close
  }

  def updStatus(txid: ByteVector32, depth: Long, updatedStamp: Long, doubleSpent: Boolean): Unit = {
    db.change(BtcTxTable.updStatusSql, depth: JLong, if (doubleSpent) 1L: JLong else 0L: JLong, updatedStamp: JLong, txid.toHex)
    DbStreams.next(DbStreams.txDbStream)
  }

  def addTx(tx: Transaction, depth: Long, received: Satoshi, sent: Satoshi, fee: Satoshi, xPubs: Seq[ExtendedPublicKey],
            description: BtcDescription, isIncoming: Long, fiatRateSnap: Fiat2Btc, stamp: Long): Unit = {
    val newSqlPQ = db.makePreparedQuery(BtcTxTable.newSql)
    db.change(newSqlPQ, tx.toString, tx.txid.toHex, xPubs.toJson.compactPrint /* WHICH WALLETS IS IT FROM */, depth: JLong,
      received.toLong: JLong, sent.toLong: JLong, fee.toLong: JLong, stamp: JLong /* SEEN */, stamp: JLong /* UPDATED */,
      description.toJson.compactPrint, 0L: JLong /* USED TO BE BALANCE SNAPSHOT */, fiatRateSnap.toJson.compactPrint,
      isIncoming: JLong, 0L: JLong /* NOT DOUBLE SPENT YET */)
    DbStreams.next(DbStreams.txDbStream)
    newSqlPQ.close
  }

  def toInfo(rc: RichCursor): BtcInfo = {
    BtcInfo(txString = rc string BtcTxTable.rawTx, txidString = rc string BtcTxTable.txid, extPubsString = rc string BtcTxTable.pub, depth = rc long BtcTxTable.depth,
      receivedSat = Satoshi(rc long BtcTxTable.receivedSat), sentSat = Satoshi(rc long BtcTxTable.sentSat), feeSat = Satoshi(rc long BtcTxTable.feeSat), seenAt = rc long BtcTxTable.seenAt,
      updatedAt = rc long BtcTxTable.updatedAt, description = to[BtcDescription](rc string BtcTxTable.description), balanceSnapshot = MilliSatoshi(rc long BtcTxTable.balanceMsat),
      fiatRatesString = rc string BtcTxTable.fiatRates, incoming = rc long BtcTxTable.incoming, doubleSpent = rc long BtcTxTable.doubleSpent)
  }
}
