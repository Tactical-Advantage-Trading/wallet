package fr.acinq.eclair.blockchain.fee

import fr.acinq.bitcoin.{Satoshi, SatoshiLong}
import scala.concurrent.Future


trait FeeProvider {
  def getFeerates: Future[FeeratesPerKB]
}

object FeeratePerByte {
  def apply(feeratePerKw: FeeratePerKw): FeeratePerByte = FeeratePerByte(FeeratePerKB(feeratePerKw).feerate / 1000)
}

case class FeeratePerByte(feerate: Satoshi)

case class FeeratePerKB(feerate: Satoshi) extends Ordered[FeeratePerKB] {
  override def compare(that: FeeratePerKB): Int = feerate.compare(that.feerate)
  def max(other: FeeratePerKB): FeeratePerKB = if (this > other) this else other
  def min(other: FeeratePerKB): FeeratePerKB = if (this < other) this else other
  def toLong: Long = feerate.toLong
}

object FeeratePerKB {
  def apply(feeratePerByte: FeeratePerByte): FeeratePerKB = FeeratePerKB(feeratePerByte.feerate * 1000)
  def apply(feeratePerKw: FeeratePerKw): FeeratePerKB = FeeratePerKB(feeratePerKw.feerate * 4)
}

case class FeeratePerKw(feerate: Satoshi) extends Ordered[FeeratePerKw] {
  override def compare(that: FeeratePerKw): Int = feerate.compare(that.feerate)
  def max(other: FeeratePerKw): FeeratePerKw = if (this > other) this else other
  def min(other: FeeratePerKw): FeeratePerKw = if (this < other) this else other
  def +(other: FeeratePerKw): FeeratePerKw = FeeratePerKw(feerate + other.feerate)
  def *(d: Double): FeeratePerKw = FeeratePerKw(feerate * d)
  def *(l: Long): FeeratePerKw = FeeratePerKw(feerate * l)
  def /(l: Long): FeeratePerKw = FeeratePerKw(feerate / l)
  def toLong: Long = feerate.toLong
}

object FeeratePerKw {
  val MinimumRelayFeeRate = 1000
  val MinimumFeeratePerKw: FeeratePerKw = FeeratePerKw(253.sat)
  def apply(feeratePerKB: FeeratePerKB): FeeratePerKw = MinimumFeeratePerKw max FeeratePerKw(feeratePerKB.feerate / 4)
  def apply(feeratePerByte: FeeratePerByte): FeeratePerKw = FeeratePerKw(FeeratePerKB(feeratePerByte))
}

case class FeeratesPerKB(mempoolMinFee: FeeratePerKB, block_1: FeeratePerKB, blocks_2: FeeratePerKB, blocks_6: FeeratePerKB, blocks_12: FeeratePerKB, blocks_36: FeeratePerKB, blocks_72: FeeratePerKB, blocks_144: FeeratePerKB, blocks_1008: FeeratePerKB) {
  require(mempoolMinFee.feerate > 0.sat && block_1.feerate > 0.sat && blocks_2.feerate > 0.sat && blocks_6.feerate > 0.sat && blocks_12.feerate > 0.sat && blocks_36.feerate > 0.sat && blocks_72.feerate > 0.sat && blocks_144.feerate > 0.sat && blocks_1008.feerate > 0.sat, "all feerates must be strictly greater than 0")

  def feePerBlock(target: Int): FeeratePerKB = target match {
    case 1 => block_1
    case 2 => blocks_2
    case t if t <= 6 => blocks_6
    case t if t <= 12 => blocks_12
    case t if t <= 36 => blocks_36
    case t if t <= 72 => blocks_72
    case t if t <= 144 => blocks_144
    case _ => blocks_1008
  }
}

case class FeeratesPerKw(mempoolMinFee: FeeratePerKw, block_1: FeeratePerKw, blocks_2: FeeratePerKw, blocks_6: FeeratePerKw, blocks_12: FeeratePerKw, blocks_36: FeeratePerKw, blocks_72: FeeratePerKw, blocks_144: FeeratePerKw, blocks_1008: FeeratePerKw) {
  require(mempoolMinFee.feerate > 0.sat && block_1.feerate > 0.sat && blocks_2.feerate > 0.sat && blocks_6.feerate > 0.sat && blocks_12.feerate > 0.sat && blocks_36.feerate > 0.sat && blocks_72.feerate > 0.sat && blocks_144.feerate > 0.sat && blocks_1008.feerate > 0.sat, "all feerates must be strictly greater than 0")

  def feePerBlock(target: Int): FeeratePerKw = target match {
    case 1 => block_1
    case 2 => blocks_2
    case t if t <= 6 => blocks_6
    case t if t <= 12 => blocks_12
    case t if t <= 36 => blocks_36
    case t if t <= 72 => blocks_72
    case t if t <= 144 => blocks_144
    case _ => blocks_1008
  }
}

object FeeratesPerKw {
  def apply(feerates: FeeratesPerKB): FeeratesPerKw = FeeratesPerKw(
    mempoolMinFee = FeeratePerKw(feerates.mempoolMinFee),
    block_1 = FeeratePerKw(feerates.block_1),
    blocks_2 = FeeratePerKw(feerates.blocks_2),
    blocks_6 = FeeratePerKw(feerates.blocks_6),
    blocks_12 = FeeratePerKw(feerates.blocks_12),
    blocks_36 = FeeratePerKw(feerates.blocks_36),
    blocks_72 = FeeratePerKw(feerates.blocks_72),
    blocks_144 = FeeratePerKw(feerates.blocks_144),
    blocks_1008 = FeeratePerKw(feerates.blocks_1008))
}