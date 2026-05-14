package trading.tacticaladvantage.utils

import rx.lang.scala.Subscription
import trading.tacticaladvantage.sqlite.SQLiteData
import trading.tacticaladvantage.utils.ImplicitJsonFormats._
import trading.tacticaladvantage.{CanBeShutDown, ConnectionProvider, Tools}

object FiatRates {
  type BlockchainInfoItemMap = Map[String, BlockchainInfoItem]
  type CoinGeckoItemMap = Map[String, CoinGeckoItem]
}

abstract class FiatRates(bag: SQLiteData, label: String) extends CanBeShutDown {
  override def becomeShutDown: Unit = updater.foreach(_.unsubscribe)
  def reloadData(provider: ConnectionProvider): Tools.Fiat2Coin

  def updateInfo(newRates: Tools.Fiat2Coin): Unit = {
    info = FiatRatesInfo(newRates, info.rates, System.currentTimeMillis)
    for (lst <- listeners) lst.onFiatRates(info)
  }

  val customFiatSymbols: Map[String, String] =
    Map("usd" -> "$", "inr" -> "₹", "gbp" -> "£", "cny" -> "CN¥",
      "jpy" -> "¥", "brl" -> "R$", "eur" -> "€", "krw" -> "₩")

  var listeners: Set[FiatRatesListener] = Set {
    new FiatRatesListener {
      def onFiatRates(info: FiatRatesInfo): Unit =
        bag.putFiatRatesInfo(info, label)
    }
  }

  var updater: Option[Subscription] = None
  var info: FiatRatesInfo = bag.tryGetFiatRatesInfo(label) getOrElse {
    FiatRatesInfo(rates = Map.empty, oldRates = Map.empty, stamp = 0L)
  }
}

class BtcFiatRates(bag: SQLiteData) extends FiatRates(bag, SQLiteData.LABEL_BTC_FIAT_RATES) {
  def reloadData(provider: ConnectionProvider) = fr.acinq.eclair.secureRandom nextInt 2 match {
    case 0 => to[CoinGecko](provider.get("https://api.coingecko.com/api/v3/exchange_rates").string).rates.map { case (code, item) => code.toLowerCase -> item.value }
    case 1 => to[FiatRates.BlockchainInfoItemMap](provider.get("https://blockchain.info/ticker").string).map { case (code, item) => code.toLowerCase -> item.last }
  }
}

class EcxFiatRates(bag: SQLiteData) extends FiatRates(bag, SQLiteData.LABEL_ECX_FIAT_RATES) {
  def reloadData(provider: ConnectionProvider) = Map("usd" -> 30D)
}

trait FiatRatesListener {
  def onFiatRates(rates: FiatRatesInfo): Unit
}

case class CoinGeckoItem(value: Double)
case class BlockchainInfoItem(last: Double)
case class CoinGecko(rates: FiatRates.CoinGeckoItemMap)

case class FiatRatesInfo(rates: Tools.Fiat2Coin, oldRates: Tools.Fiat2Coin, stamp: Long) {
  def pctDifference(code: String): Option[String] = List(rates get code, oldRates get code) match {
    case Some(fresh) :: Some(old) :: Nil if fresh > old + old / 200 => Some(s"▲ ${Denomination.formatFiatShort format pctChange(fresh, old).abs}%")
    case Some(fresh) :: Some(old) :: Nil if fresh < old - old / 200 => Some(s"▼ ${Denomination.formatFiatShort format pctChange(fresh, old).abs}%")
    case _ => None
  }

  def pctChange(fresh: Double, old: Double): Double = (fresh - old) / old * 100
}
