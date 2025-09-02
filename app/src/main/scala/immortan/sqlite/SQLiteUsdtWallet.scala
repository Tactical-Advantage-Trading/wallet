package immortan.sqlite

import immortan.sqlite.CompleteUsdtWalletInfo._

object CompleteUsdtWalletInfo {
  val NOADDRESS: String = "noaddress"
  val NOIDENTITY: String = "0x000000000000"
  val DUST_THRESHOLD: BigDecimal = 0.01
}

case class CompleteUsdtWalletInfo(address: String, xPriv: String, label: String, lastBalance: String = "0", lastNonce: String = "0", chainTip: Long = 0) {
  override def equals(other: Any): Boolean = other match { case that: CompleteUsdtWalletInfo => xPriv == that.xPriv case _ => false }
  override def hashCode: Int = xPriv.hashCode

  lazy val lcAddress: String = address.toLowerCase
  lazy val lastBalanceDecimal: BigDecimal = BigDecimal(lastBalance)
  lazy val lastBalanceDust: BigDecimal = lastBalanceDecimal % DUST_THRESHOLD
  lazy val lastBalanceNoDust: BigDecimal = lastBalanceDecimal - lastBalanceDust
  lazy val isDust: Boolean = lastBalanceDecimal <= DUST_THRESHOLD * 2
}

class SQLiteUsdtWallet(val db: DBInterface) {
  def remove(master: String): Unit = db.change(UsdtWalletTable.killSql, master)

  def addUpdateWallet(info: CompleteUsdtWalletInfo): Unit =
    db.change(UsdtWalletTable.newUpdSql, info.address, info.xPriv, info.lastBalance,
      info.lastNonce, info.chainTip: java.lang.Long, info.label)

  def listWallets: Iterable[CompleteUsdtWalletInfo] = db.select(UsdtWalletTable.selectSql).iterable { rc =>
    CompleteUsdtWalletInfo(rc string UsdtWalletTable.address, rc string UsdtWalletTable.xPriv, rc string BtcWalletTable.label,
      rc string UsdtWalletTable.lastBalance, rc string UsdtWalletTable.lastNonce, rc long UsdtWalletTable.lastTip)
  }
}
