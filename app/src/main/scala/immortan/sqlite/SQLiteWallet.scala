package immortan.sqlite

import fr.acinq.bitcoin.Crypto.PublicKey
import fr.acinq.bitcoin.DeterministicWallet.ExtendedPrivateKey
import fr.acinq.bitcoin.Satoshi
import fr.acinq.eclair.blockchain.electrum.PersistentData
import fr.acinq.eclair.blockchain.electrum.db.sqlite.SqliteWalletDb.persistentDataCodec
import immortan.utils.ImplicitJsonFormats._
import scodec.bits.ByteVector
import spray.json._

case class SigningWallet(walletType: String, attachedMaster: Option[ExtendedPrivateKey] = None, masterFingerprint: Option[Long] = None)
case class CompleteWalletInfo(core: SigningWallet, initData: ByteVector, lastBalance: Satoshi, label: String, isCoinControlOn: Boolean)

class SQLiteWallet(val db: DBInterface) {
  // Specifically do not use info.data because it may be empty ByteVector
  def addWallet(info: CompleteWalletInfo, data: ByteVector, pub: PublicKey): Unit = {
    db.change(WalletTable.newSql, info.core.toJson.compactPrint, pub.toString,
      data.toArray, info.lastBalance.toLong: java.lang.Long, info.label)
    DbStreams.next(DbStreams.walletStream)
  }

  def persist(data: PersistentData, lastBalance: Satoshi, pub: PublicKey): Unit =
    db.change(WalletTable.updSql, persistentDataCodec.encode(data).require.toByteArray,
      lastBalance.toLong: java.lang.Long, pub.toString)

  def remove(pub: PublicKey): Unit = {
    db.change(WalletTable.killSql, pub.toString)
    DbStreams.next(DbStreams.walletStream)
  }

  def listWallets: Iterable[CompleteWalletInfo] = db.select(WalletTable.selectSql).iterable { rc =>
    CompleteWalletInfo(to[SigningWallet](rc string WalletTable.info), rc byteVec WalletTable.data,
      Satoshi(rc long WalletTable.lastBalance), rc string WalletTable.label, isCoinControlOn = false)
  }
}
