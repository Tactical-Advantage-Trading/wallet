package immortan.sqlite

import fr.acinq.bitcoin.DeterministicWallet.ExtendedPrivateKey
import immortan.utils.ImplicitJsonFormats._
import spray.json._

object CompleteUsdtWalletInfo { val NOADDRESS = "noaddress" }

case class CompleteUsdtWalletInfo(address: String, master: ExtendedPrivateKey, lastBalance: String, lastNonce: String, chainTip: Long, label: String)

class SQLiteUsdtWallet(val db: DBInterface) {
  def remove(master: ExtendedPrivateKey): Unit =
    db.change(UsdtWalletTable.killSql, master.toJson.compactPrint)

  def addWallet(info: CompleteUsdtWalletInfo): Unit =
    db.change(UsdtWalletTable.newSql, info.address, info.master.toJson.compactPrint,
      info.lastBalance, info.lastNonce, info.chainTip: java.lang.Long, info.label)

  def persist(lastBalance: String, lastNonce: String, chainTip: Long, address: String): Unit =
    db.change(UsdtWalletTable.updSql, lastBalance, lastNonce, chainTip: java.lang.Long, address)

  def updateLabel(label: String, master: ExtendedPrivateKey): Unit =
    db.change(UsdtWalletTable.updLabelSql, label, master.toJson.compactPrint)

  def updateAddress(address: String, master: ExtendedPrivateKey): Unit =
    db.change(UsdtWalletTable.updAddressSql, address, master.toJson.compactPrint)

  def updateTip(chainTip: Long, address: String): Unit =
    db.change(UsdtWalletTable.updTipSql, chainTip: java.lang.Long)

  def listWallets: Iterable[CompleteUsdtWalletInfo] = db.select(UsdtWalletTable.selectSql).iterable { rc =>
    CompleteUsdtWalletInfo(rc string UsdtWalletTable.address, to[ExtendedPrivateKey](rc string UsdtWalletTable.xPriv),
      rc string UsdtWalletTable.lastBalance, rc string UsdtWalletTable.lastNonce, rc long UsdtWalletTable.lastTip,
      rc string BtcWalletTable.label)
  }
}
