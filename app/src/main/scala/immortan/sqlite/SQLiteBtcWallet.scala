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
case class CompleteBtcWalletInfo(core: SigningWallet, initData: ByteVector, lastBalance: Satoshi, label: String, isCoinControlOn: Boolean)

class SQLiteBtcWallet(val db: DBInterface) {
  // Specifically do not use info.data because it may be empty ByteVector
  def addWallet(info: CompleteBtcWalletInfo, data: ByteVector, pub: PublicKey): Unit =
    db.change(BtcWalletTable.newSql, info.core.toJson.compactPrint, pub.toString,
      data.toArray, info.lastBalance.toLong: java.lang.Long, info.label)

  def persist(data: PersistentData, lastBalance: Satoshi, pub: PublicKey): Unit =
    db.change(BtcWalletTable.updSql, persistentDataCodec.encode(data).require.toByteArray,
      lastBalance.toLong: java.lang.Long, pub.toString)

  def updateLabel(label: String, pub: PublicKey): Unit =
    db.change(BtcWalletTable.updLabelSql, label, pub.toString)

  def remove(pub: PublicKey): Unit =
    db.change(BtcWalletTable.killSql, pub.toString)

  def listWallets: Iterable[CompleteBtcWalletInfo] = db.select(BtcWalletTable.selectSql).iterable { rc =>
    CompleteBtcWalletInfo(to[SigningWallet](rc string BtcWalletTable.info), rc byteVec BtcWalletTable.data,
      Satoshi(rc long BtcWalletTable.lastBalance), rc string BtcWalletTable.label, isCoinControlOn = false)
  }
}
