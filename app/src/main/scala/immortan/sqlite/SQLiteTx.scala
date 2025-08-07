package immortan.sqlite

import java.lang.{Long => JLong}

import fr.acinq.bitcoin.DeterministicWallet.ExtendedPublicKey
import fr.acinq.bitcoin.{ByteVector32, Satoshi, Transaction}
import fr.acinq.eclair.MilliSatoshi
import immortan.crypto.Tools.Fiat2Btc
import immortan.utils.ImplicitJsonFormats._
import immortan.{TxDescription, TxInfo}
import spray.json._


class SQLiteTx(val db: DBInterface) {
  def listRecentTxs(limit: Int): RichCursor = db.select(TxTable.selectRecentSql, limit.toString)
  def searchTransactions(rawSearchQuery: String): RichCursor = db.search(TxTable.searchSql, rawSearchQuery.toLowerCase)

  def listAllDescriptions: Map[String, TxDescription] =
    db.select(TxTable.selectRecentSql, 10000.toString).iterable { rc =>
      val description = to[TxDescription](rc string TxTable.description)
      (rc string TxTable.txid, description)
    }.toMap

  def addSearchableTransaction(search: String, txid: ByteVector32): Unit = {
    val newVirtualSqlPQ = db.makePreparedQuery(TxTable.newVirtualSql)
    db.change(newVirtualSqlPQ, search.toLowerCase, txid.toHex)
    newVirtualSqlPQ.close
  }

  def updDescription(description: TxDescription, txid: ByteVector32): Unit = db txWrap {
    val updateDescriptionSqlPQ = db.makePreparedQuery(TxTable.updateDescriptionSql)
    db.change(updateDescriptionSqlPQ, description.toJson.compactPrint, txid.toHex)
    for (label <- description.label) addSearchableTransaction(label, txid)
    DbStreams.next(DbStreams.txDbStream)
    updateDescriptionSqlPQ.close
  }

  def updStatus(txid: ByteVector32, depth: Long, updatedStamp: Long, doubleSpent: Boolean): Unit = {
    db.change(TxTable.updStatusSql, depth: JLong, if (doubleSpent) 1L: JLong else 0L: JLong, updatedStamp: JLong, txid.toHex)
    DbStreams.next(DbStreams.txDbStream)
  }

  def addTx(tx: Transaction, depth: Long, received: Satoshi, sent: Satoshi, fee: Satoshi, xPubs: Seq[ExtendedPublicKey],
            description: TxDescription, isIncoming: Long, fiatRateSnap: Fiat2Btc, stamp: Long): Unit = {

    val newSqlPQ = db.makePreparedQuery(TxTable.newSql)
    db.change(newSqlPQ, tx.toString, tx.txid.toHex, xPubs.toJson.compactPrint /* WHICH WALLETS IS IT FROM */, depth: JLong,
      received.toLong: JLong, sent.toLong: JLong, fee.toLong: JLong, stamp: JLong /* SEEN */, stamp: JLong /* UPDATED */,
      description.toJson.compactPrint, 0L: JLong /* USED TO BE BALANCE SNAPSHOT */, fiatRateSnap.toJson.compactPrint,
      isIncoming: JLong, 0L: JLong /* NOT DOUBLE SPENT YET */)
    DbStreams.next(DbStreams.txDbStream)
    newSqlPQ.close
  }

  def toTxInfo(rc: RichCursor): TxInfo = {
    TxInfo(txString = rc string TxTable.rawTx, txidString = rc string TxTable.txid, extPubsString = rc string TxTable.pub, depth = rc long TxTable.depth,
      receivedSat = Satoshi(rc long TxTable.receivedSat), sentSat = Satoshi(rc long TxTable.sentSat), feeSat = Satoshi(rc long TxTable.feeSat), seenAt = rc long TxTable.seenAt,
      updatedAt = rc long TxTable.updatedAt, description = to[TxDescription](rc string TxTable.description), balanceSnapshot = MilliSatoshi(rc long TxTable.balanceMsat),
      fiatRatesString = rc string TxTable.fiatRates, incoming = rc long TxTable.incoming, doubleSpent = rc long TxTable.doubleSpent)
  }
}
