package immortan.sqlite

import fr.acinq.bitcoin.Crypto.PublicKey
import fr.acinq.bitcoin.Satoshi
import fr.acinq.eclair.blockchain.electrum.PersistentData
import fr.acinq.eclair.blockchain.electrum.db.sqlite.SqliteWalletDb.persistentDataCodec
import fr.acinq.eclair.blockchain.electrum.db.{ChainWalletInfo, CompleteChainWalletInfo, WalletDb}
import immortan.utils.ImplicitJsonFormats._
import scodec.bits.ByteVector
import spray.json._


class SQLiteChainWallet(val db: DBInterface) extends WalletDb {
  def remove(pub: PublicKey): Unit = db.change(ChainWalletTable.killSql, pub.toString)

  // Specifically do not use info.data because it may be empty ByteVector
  def addChainWallet(info: CompleteChainWalletInfo, data: ByteVector, pub: PublicKey): Unit =
    db.change(ChainWalletTable.newSql, info.core.toJson.compactPrint, pub.toString,
      data.toArray, info.lastBalance.toLong: java.lang.Long, info.label)

  def persist(data: PersistentData, lastBalance: Satoshi, pub: PublicKey): Unit =
    db.change(ChainWalletTable.updSql, persistentDataCodec.encode(data).require.toByteArray,
      lastBalance.toLong: java.lang.Long, pub.toString)

  def updateLabel(label: String, pub: PublicKey): Unit = db.change(ChainWalletTable.updLabelSql, label, pub.toString)

  def listWallets: Iterable[CompleteChainWalletInfo] = db.select(ChainWalletTable.selectSql).iterable { rc =>
    CompleteChainWalletInfo(to[ChainWalletInfo](rc string ChainWalletTable.info), rc byteVec ChainWalletTable.data,
      Satoshi(rc long ChainWalletTable.lastBalance), rc string ChainWalletTable.label, isCoinControlOn = false)
  }
}
