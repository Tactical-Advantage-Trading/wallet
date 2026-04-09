package trading.tacticaladvantage.sqlite

import fr.acinq.bitcoin.DeterministicWallet.ExtendedPublicKey
import fr.acinq.bitcoin.{ByteVector32, Satoshi, Transaction}
import fr.acinq.eclair.MilliSatoshi
import trading.tacticaladvantage.Tools.Fiat2Coin
import trading.tacticaladvantage.utils.ImplicitJsonFormats._
import spray.json._
import trading.tacticaladvantage.{CoinDescription, CoinDetails}

import java.lang.{Long => JLong}

object SQLiteTx {
  def reify(rc: RichCursor): CoinDetails =
    CoinDetails(txString = rc string TxTable.rawTx, identity = rc string TxTable.txid, extPubsString = rc string TxTable.pub, depth = rc long TxTable.depth,
      receivedSat = Satoshi(rc long TxTable.receivedSat), sentSat = Satoshi(rc long TxTable.sentSat), feeSat = Satoshi(rc long TxTable.feeSat), seenAt = rc long TxTable.seenAt,
      updatedAt = rc long TxTable.updatedAt, description = to[CoinDescription](rc string TxTable.description), balanceSnapshot = MilliSatoshi(rc long TxTable.balanceMsat),
      fiatRatesString = rc string TxTable.fiatRates, incoming = rc long TxTable.incoming, doubleSpent = rc long TxTable.doubleSpent)
}

class SQLiteTx(val db: DBInterface) {
  def listRecentTxs(limit: Int): RichCursor = db.select(TxTable.selectRecentSql, limit.toString)

  def addSearchableTransaction(search: String, txid: ByteVector32): Unit = {
    val newVirtualSqlPQ = db.makePreparedQuery(TxTable.newVirtualSql)
    db.change(newVirtualSqlPQ, search.toLowerCase, txid.toHex)
    newVirtualSqlPQ.close
  }

  def updDescription(description: CoinDescription, txid: ByteVector32): Unit = db txWrap {
    val updateDescriptionSqlPQ = db.makePreparedQuery(TxTable.updateDescriptionSql)
    db.change(updateDescriptionSqlPQ, description.toJson.compactPrint, txid.toHex)
    for (label <- description.label) addSearchableTransaction(label, txid)
    DbStreams.next(DbStreams.txStream)
    updateDescriptionSqlPQ.close
  }

  def updStatus(txid: ByteVector32, depth: Long, updatedStamp: Long, doubleSpent: Boolean): Unit = {
    db.change(TxTable.updStatusSql, depth: JLong, if (doubleSpent) 1L: JLong else 0L: JLong, updatedStamp: JLong, txid.toHex)
    DbStreams.next(DbStreams.txStream)
  }

  def addTx(tx: Transaction, depth: Long, received: Satoshi, sent: Satoshi, fee: Satoshi, xPubs: Seq[ExtendedPublicKey],
            description: CoinDescription, isIncoming: Long, fiatRateSnap: Fiat2Coin, stamp: Long): Unit = {
    val newSqlPQ = db.makePreparedQuery(TxTable.newSql)
    db.change(newSqlPQ, tx.toString, tx.txid.toHex, xPubs.toJson.compactPrint /* WHICH WALLETS IS IT FROM */, depth: JLong,
      received.toLong: JLong, sent.toLong: JLong, fee.toLong: JLong, stamp: JLong /* SEEN */, stamp: JLong /* UPDATED */,
      description.toJson.compactPrint, 0L: JLong /* USED TO BE BALANCE SNAPSHOT */, fiatRateSnap.toJson.compactPrint,
      isIncoming: JLong, 0L: JLong /* NOT DOUBLE SPENT YET */)
    DbStreams.next(DbStreams.txStream)
    newSqlPQ.close
  }
}
