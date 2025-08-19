package immortan.utils

import fr.acinq.eclair.blockchain.electrum.ElectrumWallet
import immortan.crypto.{CanBeShutDown, Tools}
import immortan.sqlite.SQLiteData
import immortan.utils.ImplicitJsonFormats._


object FiatRates {
  type BlockchainInfoItemMap = Map[String, BlockchainInfoItem]
  type CoinGeckoItemMap = Map[String, CoinGeckoItem]
}

class FiatRates(bag: SQLiteData) extends CanBeShutDown {
  override def becomeShutDown: Unit = listeners = Set.empty

  val customFiatSymbols: Map[String, String] = Map("usd" -> "$", "inr" -> "₹", "gbp" -> "£", "cny" -> "CN¥", "jpy" -> "¥", "brl" -> "R$", "eur" -> "€", "krw" -> "₩")

  val universallySupportedSymbols: Map[String, String] = Map("usd" -> "US Dollar", "eur" -> "Euro", "jpy" -> "Japanese Yen", "cny" -> "Chinese Yuan", "inr" -> "Indian Rupee", "cad" -> "Canadian Dollar",
    "brl" -> "Real Brasileiro", "czk" -> "Česká Koruna", "gbp" -> "Pound Sterling", "aud" -> "Australian Dollar", "try" -> "Turkish Lira", "nzd" -> "New Zealand Dollar", "thb" -> "Thai Baht",
    "twd" -> "New Taiwan Dollar", "krw" -> "South Korean won", "clp" -> "Chilean Peso", "sgd" -> "Singapore Dollar", "hkd" -> "Hong Kong Dollar", "pln" -> "Polish złoty",
    "dkk" -> "Danish Krone", "sek" -> "Swedish Krona", "chf" -> "Swiss franc", "huf" -> "Hungarian forint")

  def reloadData: Tools.Fiat2Btc = fr.acinq.eclair.secureRandom nextInt 2 match {
    case 0 => to[CoinGecko](ElectrumWallet.connectionProvider.get("https://api.coingecko.com/api/v3/exchange_rates").string).rates.map { case (code, item) => code.toLowerCase -> item.value }
    case 1 => to[FiatRates.BlockchainInfoItemMap](ElectrumWallet.connectionProvider.get("https://blockchain.info/ticker").string).map { case (code, item) => code.toLowerCase -> item.last }
  }

  def updateInfo(newRates: Tools.Fiat2Btc): Unit = {
    info = FiatRatesInfo(newRates, info.rates, System.currentTimeMillis)
    for (lst <- listeners) lst.onFiatRates(info)
  }

  var listeners: Set[FiatRatesListener] = Set.empty
  var info: FiatRatesInfo = bag.tryGetFiatRatesInfo getOrElse {
    FiatRatesInfo(rates = Map.empty, oldRates = Map.empty, stamp = 0L)
  }
}

trait FiatRatesListener {
  def onFiatRates(rates: FiatRatesInfo): Unit
}

case class CoinGeckoItem(value: Double)
case class BlockchainInfoItem(last: Double)
case class CoinGecko(rates: FiatRates.CoinGeckoItemMap)

case class FiatRatesInfo(rates: Tools.Fiat2Btc, oldRates: Tools.Fiat2Btc, stamp: Long) {
  def pctDifference(code: String): Option[String] = List(rates get code, oldRates get code) match {
    case Some(fresh) :: Some(old) :: Nil if fresh > old + old / 200 => Some(s"▲ ${Denomination.formatFiatShort format pctChange(fresh, old).abs}%")
    case Some(fresh) :: Some(old) :: Nil if fresh < old - old / 200 => Some(s"▼ ${Denomination.formatFiatShort format pctChange(fresh, old).abs}%")
    case _ => None
  }

  def pctChange(fresh: Double, old: Double): Double = (fresh - old) / old * 100
}
