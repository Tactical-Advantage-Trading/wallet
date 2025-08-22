package immortan.sqlite

import immortan.UsdtInfo

object CompleteUsdtWalletInfo { val NOADDRESS = "noaddress" }
case class CompleteUsdtWalletInfo(address: String, xPriv: String, label: String, lastBalance: String = "0", lastNonce: String = "0", chainTip: Long = 0) {
  override def equals(other: Any): Boolean = other match { case that: CompleteUsdtWalletInfo => xPriv == that.xPriv case _ => false }
  def isRelatedToInfo(info: UsdtInfo): Boolean = address == info.description.toAddr || address == info.description.fromAddr
  override def hashCode: Int = xPriv.hashCode
}

class SQLiteUsdtWallet(val db: DBInterface) {
  def remove(master: String): Unit = db.change(UsdtWalletTable.killSql, master)

  def addUpdateWallet(info: CompleteUsdtWalletInfo): Unit =
    db.change(UsdtWalletTable.newUpdSql, info.address, info.xPriv,
      info.lastBalance, info.lastNonce, info.chainTip: java.lang.Long,
      info.label)

  def listWallets: Iterable[CompleteUsdtWalletInfo] = db.select(UsdtWalletTable.selectSql).iterable { rc =>
    CompleteUsdtWalletInfo(rc string UsdtWalletTable.address, rc string UsdtWalletTable.xPriv, rc string BtcWalletTable.label,
      rc string UsdtWalletTable.lastBalance, rc string UsdtWalletTable.lastNonce, rc long UsdtWalletTable.lastTip)
  }
}
