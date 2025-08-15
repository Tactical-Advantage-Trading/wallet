package immortan.sqlite

object CompleteUsdtWalletInfo { val NOADDRESS = "noaddress" }
case class CompleteUsdtWalletInfo(address: String, xPriv: String, label: String, lastBalance: String = "0", lastNonce: String = "0", chainTip: Long = 0) {
  override def equals(other: Any): Boolean = other match { case that: CompleteUsdtWalletInfo => xPriv == that.xPriv case _ => false }
  override def hashCode: Int = xPriv.hashCode
}

class SQLiteUsdtWallet(val db: DBInterface) {
  def remove(master: String): Unit = db.change(UsdtWalletTable.killSql, master)

  def addWallet(info: CompleteUsdtWalletInfo): Unit =
    db.change(UsdtWalletTable.newSql, info.address, info.xPriv,
      info.lastBalance, info.lastNonce, info.chainTip: java.lang.Long,
      info.label)

  def persist(lastBalance: String, lastNonce: String, chainTip: Long, address: String): Unit =
    db.change(UsdtWalletTable.updSql, lastBalance, lastNonce, chainTip: java.lang.Long, address)

  def updateLabel(label: String, master: String): Unit = db.change(UsdtWalletTable.updLabelSql, label, master)

  def updateAddress(address: String, master: String): Unit = db.change(UsdtWalletTable.updAddressSql, address, master)

  def listWallets: Iterable[CompleteUsdtWalletInfo] = db.select(UsdtWalletTable.selectSql).iterable { rc =>
    CompleteUsdtWalletInfo(rc string UsdtWalletTable.address, rc string UsdtWalletTable.xPriv, rc string BtcWalletTable.label,
      rc string UsdtWalletTable.lastBalance, rc string UsdtWalletTable.lastNonce, rc long UsdtWalletTable.lastTip)
  }
}
