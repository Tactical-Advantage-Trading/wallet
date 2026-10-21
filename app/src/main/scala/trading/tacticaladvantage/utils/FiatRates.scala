package trading.tacticaladvantage.utils

import rx.lang.scala.Subscription
import trading.tacticaladvantage.sqlite.SQLiteData
import trading.tacticaladvantage.utils.ImplicitJsonFormats._
import trading.tacticaladvantage.{CanBeShutDown, ConnectionProvider, Tools}
import spray.json.JsonFormat
import scala.util.Try

object FiatRates {
  type BlockchainInfoItemMap = Map[String, BlockchainInfoItem]
  type JupiterInfoItemMap = Map[String, JupiterPrice]
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
  def reloadData(provider: ConnectionProvider) = {
    val data = provider.get("https://blockchain.info/ticker")
    to[FiatRates.BlockchainInfoItemMap](data.string).map {
      case (code, item) => (code.toLowerCase, item.last)
    }
  }
}

class EcxFiatRates(bag: SQLiteData) extends FiatRates(bag, SQLiteData.LABEL_ECX_FIAT_RATES) {
  private val poolAddress = "nNKg814Wq3uTkoG4fM8LzvBQv4Fu2iCgKFmK2YmPQzM"
  private val WbEcxMint = "EVHqNdzjCupKi4rQkbuYw52sa1m8A7jeUAMP23S9AVVq"
  private val UsdcMint = "EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"
  private val impliedEcxMultiplier = 50D

  private def positive(value: Double): Double = {
    require(value > 0D && !value.isNaN && !value.isInfinity, "Invalid price")
    value
  }

  private def load[T: JsonFormat](provider: ConnectionProvider, url: String): T = {
    val body = provider.get(url)
    try to[T](body.string)
    finally body.close
  }

  private def reload(provider: ConnectionProvider) = Try {
    val url = s"https://lite-api.jup.ag/price/v3?ids=$WbEcxMint"
    val quotes = load[FiatRates.JupiterInfoItemMap](provider, url)
    positive(quotes(WbEcxMint).usdPrice)
  } recover { case _ =>
    val url = s"https://api.orca.so/v2/solana/pools/$poolAddress"
    val pool = load[OrcaResponse](provider, url).data
    val raw = positive(pool.price.toDouble)

    (pool.tokenA.address, pool.tokenB.address) match {
      case (UsdcMint, WbEcxMint) => positive(1D / raw)
      case (WbEcxMint, UsdcMint) => raw
      case _ => throw new Exception
    }
  }

  def reloadData(provider: ConnectionProvider): Tools.Fiat2Coin =
    Map("usd" -> reload(provider).map(_ * impliedEcxMultiplier).map(positive).get)
}

trait FiatRatesListener {
  def onFiatRates(rates: FiatRatesInfo): Unit
}

case class BlockchainInfoItem(last: Double)

case class JupiterPrice(usdPrice: Double)
case class OrcaPool(price: String, tokenA: OrcaToken, tokenB: OrcaToken)
case class OrcaResponse(data: OrcaPool)
case class OrcaToken(address: String)

case class FiatRatesInfo(rates: Tools.Fiat2Coin, oldRates: Tools.Fiat2Coin, stamp: Long) {
  def pctDifference(code: String): Option[String] = List(rates get code, oldRates get code) match {
    case Some(fresh) :: Some(old) :: Nil if fresh > old + old / 200 => Some(s"▲ ${Denomination.formatFiatShort format pctChange(fresh, old).abs}%")
    case Some(fresh) :: Some(old) :: Nil if fresh < old - old / 200 => Some(s"▼ ${Denomination.formatFiatShort format pctChange(fresh, old).abs}%")
    case _ => None
  }

  def pctChange(fresh: Double, old: Double): Double = (fresh - old) / old * 100
}
