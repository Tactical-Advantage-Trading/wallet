package trading.tacticaladvantage.utils

import fr.acinq.bitcoin._
import fr.acinq.eclair.blockchain.fee._
import rx.lang.scala.Subscription
import trading.tacticaladvantage.sqlite.SQLiteData
import trading.tacticaladvantage.utils.FeeRates._
import trading.tacticaladvantage.utils.ImplicitJsonFormats._
import trading.tacticaladvantage.{CanBeShutDown, ConnectionProvider}

object FeeRates {
  val minPerKw: FeeratePerKw = FeeratePerKw(1000L.sat)

  val defaultFeerates: FeeratesPerKB =
    FeeratesPerKB(
      mempoolMinFee = FeeratePerKB(5000.sat),
      block_1 = FeeratePerKB(210000.sat),
      blocks_2 = FeeratePerKB(180000.sat),
      blocks_6 = FeeratePerKB(150000.sat),
      blocks_12 = FeeratePerKB(110000.sat),
      blocks_36 = FeeratePerKB(50000.sat),
      blocks_72 = FeeratePerKB(20000.sat),
      blocks_144 = FeeratePerKB(15000.sat),
      blocks_1008 = FeeratePerKB(5000.sat)
    )

  def smoothedFeeratesPerKw(history: List[FeeratesPerKB] = Nil): FeeratesPerKw =
    FeeratesPerKw(
      FeeratesPerKB(
        FeeratePerKB(Statistics.meanBy(history)(_.mempoolMinFee.toLong).toLong.sat),
        FeeratePerKB(Statistics.meanBy(history)(_.block_1.toLong).toLong.sat),
        FeeratePerKB(Statistics.meanBy(history)(_.blocks_2.toLong).toLong.sat),
        FeeratePerKB(Statistics.meanBy(history)(_.blocks_6.toLong).toLong.sat),
        FeeratePerKB(Statistics.meanBy(history)(_.blocks_12.toLong).toLong.sat),
        FeeratePerKB(Statistics.meanBy(history)(_.blocks_36.toLong).toLong.sat),
        FeeratePerKB(Statistics.meanBy(history)(_.blocks_72.toLong).toLong.sat),
        FeeratePerKB(Statistics.meanBy(history)(_.blocks_144.toLong).toLong.sat),
        FeeratePerKB(Statistics.meanBy(history)(_.blocks_1008.toLong).toLong.sat)
      )
    )
}

abstract class FeeRates(bag: SQLiteData, label: String) extends CanBeShutDown {
  override def becomeShutDown: Unit = updater.foreach(_.unsubscribe)
  def reloadData(provider: ConnectionProvider): FeeratesPerKB

  def updateInfo(newPerKB: FeeratesPerKB): Unit = {
    val history1 = (newPerKB :: info.history).diff(defaultFeerates :: Nil).take(2)
    info = FeeRatesInfo(smoothedFeeratesPerKw(history1), history1, System.currentTimeMillis)
    for (lst <- listeners) lst.onFeeRates(info)
  }

  var listeners: Set[FeeRatesListener] = Set {
    new FeeRatesListener {
      def onFeeRates(info: FeeRatesInfo): Unit =
        bag.putFeeRatesInfo(info, label)
    }
  }

  var updater: Option[Subscription] = None
  var info: FeeRatesInfo = bag.tryGetFeeRatesInfo(label) getOrElse {
    FeeRatesInfo(FeeratesPerKw(defaultFeerates), history = Nil, stamp = 0L)
  }
}

class BtcFeeRates(bag: SQLiteData) extends FeeRates(bag, SQLiteData.LABEL_BTC_FEE_RATES) {
  def reloadData(provider: ConnectionProvider) = fr.acinq.eclair.secureRandom nextInt 3 match {
    case 0 => new EsploraFeeProvider("https://blockstream.info/api/fee-estimates").provide(provider)
    case 1 => new EsploraFeeProvider("https://mempool.space/api/fee-estimates").provide(provider)
    case _ => BitgoFeeProvider.provide(provider)
  }
}

class EcxFeeRates(bag: SQLiteData) extends FeeRates(bag, SQLiteData.LABEL_ECX_FEE_RATES) {
  def reloadData(provider: ConnectionProvider) = FeeratesPerKB(
    mempoolMinFee = FeeratePerKB(1L.sat),
    block_1 = FeeratePerKB(1L.sat),
    blocks_2 = FeeratePerKB(1L.sat),
    blocks_6 = FeeratePerKB(1L.sat),
    blocks_12 = FeeratePerKB(1L.sat),
    blocks_36 = FeeratePerKB(1L.sat),
    blocks_72 = FeeratePerKB(1L.sat),
    blocks_144 = FeeratePerKB(1L.sat),
    blocks_1008 = FeeratePerKB(1L.sat)
  )
}

case class FeeRatesInfo(smoothed: FeeratesPerKw, history: List[FeeratesPerKB], stamp: Long) {
  private val targets = FeeTargets(fundingBlockTarget = 36, commitmentBlockTarget = 12, mutualCloseBlockTarget = 72, claimMainBlockTarget = 144)
  private val estimator = new FeeEstimator { override def getFeeratePerKw(target: Int): FeeratePerKw = smoothed.feePerBlock(target) max minPerKw }
  val onChainFeeConf: OnChainFeeConf = OnChainFeeConf(targets, estimator)
}

trait FeeRatesListener {
  def onFeeRates(rates: FeeRatesInfo): Unit
}

trait FeeRatesProvider {
  def provide(provider: ConnectionProvider): FeeratesPerKB
  val url: String
}

// Esplora

class EsploraFeeProvider(val url: String) extends FeeRatesProvider {
  type EsploraFeeStructure = Map[String, Long]

  def provide(provider: ConnectionProvider): FeeratesPerKB = {
    val structure = to[EsploraFeeStructure](provider.get(url).string)

    FeeratesPerKB(
      mempoolMinFee = extractFeerate(structure, 1008),
      block_1 = extractFeerate(structure, 1),
      blocks_2 = extractFeerate(structure, 2),
      blocks_6 = extractFeerate(structure, 6),
      blocks_12 = extractFeerate(structure, 12),
      blocks_36 = extractFeerate(structure, 36),
      blocks_72 = extractFeerate(structure, 72),
      blocks_144 = extractFeerate(structure, 144),
      blocks_1008 = extractFeerate(structure, 1008)
    )
  }

  // First we keep only fee ranges with a max block delay below the limit
  // out of all the remaining fee ranges, we select the one with the minimum higher bound
  def extractFeerate(structure: EsploraFeeStructure, maxBlockDelay: Int): FeeratePerKB = {
    val belowLimit = structure.filterKeys(_.toInt <= maxBlockDelay).values
    FeeratePerKB(belowLimit.min.sat * 1000L)
  }
}

// BitGo

case class BitGoFeeRateStructure(feeByBlockTarget: Map[String, Long], feePerKb: Long)

object BitgoFeeProvider extends FeeRatesProvider {
  val url = "https://www.bitgo.com/api/v2/btc/tx/fee"

  def provide(provider: ConnectionProvider): FeeratesPerKB = {
    val structure = to[BitGoFeeRateStructure](provider.get(url).string)

    FeeratesPerKB(
      mempoolMinFee = extractFeerate(structure, 1008),
      block_1 = extractFeerate(structure, 1),
      blocks_2 = extractFeerate(structure, 2),
      blocks_6 = extractFeerate(structure, 6),
      blocks_12 = extractFeerate(structure, 12),
      blocks_36 = extractFeerate(structure, 36),
      blocks_72 = extractFeerate(structure, 72),
      blocks_144 = extractFeerate(structure, 144),
      blocks_1008 = extractFeerate(structure, 1008)
    )
  }

  // first we keep only fee ranges with a max block delay below the limit
  // out of all the remaining fee ranges, we select the one with the minimum higher bound
  def extractFeerate(structure: BitGoFeeRateStructure, maxBlockDelay: Int): FeeratePerKB = {
    val belowLimit = structure.feeByBlockTarget.filterKeys(_.toInt <= maxBlockDelay).values
    FeeratePerKB(belowLimit.min.sat)
  }
}
