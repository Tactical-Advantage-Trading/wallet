package immortan

import java.util.Date
import fr.acinq.bitcoin.DeterministicWallet.ExtendedPublicKey
import fr.acinq.bitcoin.{ByteVector32, Satoshi, Transaction}
import fr.acinq.eclair._
import fr.acinq.eclair.blockchain.electrum.ElectrumWalletType
import immortan.crypto.Tools.{Any2Some, ExtPubKeys, Fiat2Btc, SEPARATOR, StringList}
import immortan.sqlite.CompleteBtcWalletInfo
import immortan.utils.ImplicitJsonFormats._

case class SemanticOrder(id: String, order: Long)
case class RBFParams(ofTxid: ByteVector32, mode: Long)

object SemanticOrder {
  type SemanticGroup = Seq[ItemDetails]
  private def orderIdOrBaseId(details: ItemDetails) = details.description.semanticOrder.map(_.id).getOrElse(details.identity)
  private def orderOrMaxValue(details: ItemDetails) = details.description.semanticOrder.map(_.order).getOrElse(Long.MaxValue)

  private def collapseChildren(items: SemanticGroup) = {
    items.tail.foreach(_.isExpandedItem = false)
    items.head.isExpandedItem = true
    items
  }

  def makeSemanticOrder(items: SemanticGroup): SemanticGroup =
    items.distinct.groupBy(orderIdOrBaseId).mapValues(_ sortBy orderOrMaxValue)
      .mapValues(collapseChildren).values.toList.sortBy(_.head.seenAt)(Ordering[Long].reverse)
      .flatten
}

sealed trait ItemDescription {
  val semanticOrder: Option[SemanticOrder]
  val label: Option[String]
}

sealed trait ItemDetails {
  var isExpandedItem: Boolean = true
  // We order items on UI by when they were first seen
  // We hide items depending on when they were updated
  def updatedAt: Long
  def seenAt: Long

  val date: Date = new Date(updatedAt)
  val description: ItemDescription
  val isDoubleSpent: Boolean
  val identity: String
}

// BTC Address

case class BtcAddressDescription(label: Option[String] = None) extends ItemDescription {
  val semanticOrder: Option[SemanticOrder] = None
}

case class BtcAddressInfo(ewt: ElectrumWalletType, core: CompleteBtcWalletInfo, pubKey: ExtendedPublicKey, description: BtcAddressDescription) extends ItemDetails {
  override val identity: String = ewt.textAddress(pubKey)
  override val isDoubleSpent: Boolean = false
  override def updatedAt: Long = 0L
  override def seenAt: Long = 0L
}

// BTC tx

sealed trait BtcDescription extends ItemDescription {
  def canBeCPFPd: Boolean = cpfpBy.isEmpty && cpfpOf.isEmpty
  def withNewOrderCond(order: Option[SemanticOrder] = None): BtcDescription
  def withNewLabel(label1: Option[String] = None): BtcDescription
  def withNewCPFPBy(txid: ByteVector32): BtcDescription

  def addresses: StringList
  def queryText(txid: ByteVector32): String
  val cpfpBy: Option[ByteVector32]
  val cpfpOf: Option[ByteVector32]
  val rbf: Option[RBFParams]
}

object BtcDescription {
  final val RBF_CANCEL = 1
  final val RBF_BOOST = 2
}

case class PlainBtcDescription(addresses: StringList,
                               label: Option[String] = None, semanticOrder: Option[SemanticOrder] = None,
                               cpfpBy: Option[ByteVector32] = None, cpfpOf: Option[ByteVector32] = None,
                               rbf: Option[RBFParams] = None) extends BtcDescription { me =>
  override def queryText(txid: ByteVector32): String = txid.toHex + SEPARATOR + addresses.mkString(SEPARATOR) + SEPARATOR + label.getOrElse(new String)
  override def withNewOrderCond(order: Option[SemanticOrder] = None): BtcDescription = if (semanticOrder.isDefined) me else copy(semanticOrder = order)
  override def withNewLabel(label1: Option[String] = None): BtcDescription = copy(label = label1)
  override def withNewCPFPBy(txid: ByteVector32): BtcDescription = copy(cpfpBy = txid.asSome)
}

// We do not know addresses here
case class FallbackBtcDescription(label: Option[String] = None, semanticOrder: Option[SemanticOrder] = None,
                                  cpfpBy: Option[ByteVector32] = None, cpfpOf: Option[ByteVector32] = None,
                                  rbf: Option[RBFParams] = None) extends BtcDescription {
  override def queryText(txid: ByteVector32): String = txid.toHex + SEPARATOR + label.getOrElse(new String)
  override def withNewOrderCond(order: Option[SemanticOrder] = None): BtcDescription = copy(semanticOrder = order)
  override def withNewLabel(label1: Option[String] = None): BtcDescription = copy(label = label1)
  override def withNewCPFPBy(txid: ByteVector32): BtcDescription = copy(cpfpBy = txid.asSome)
  def addresses: StringList = Nil
}

case class BtcInfo(txString: String, txidString: String, extPubsString: String, depth: Long, receivedSat: Satoshi, sentSat: Satoshi,
                   feeSat: Satoshi, seenAt: Long, updatedAt: Long, description: BtcDescription, balanceSnapshot: MilliSatoshi,
                   fiatRatesString: String, incoming: Long, doubleSpent: Long) extends ItemDetails {
  override val isDoubleSpent: Boolean = 1L == doubleSpent
  override val identity: String = txidString
  val isIncoming: Boolean = 1L == incoming
  val isConfirmed: Boolean = depth > 0

  lazy val fiatRateSnapshot: Fiat2Btc = to[Fiat2Btc](fiatRatesString)
  lazy val extPubs: ExtPubKeys = tryTo[ExtPubKeys](extPubsString).getOrElse(Nil)
  lazy val txid: ByteVector32 = ByteVector32.fromValidHex(txidString)
  lazy val tx: Transaction = Transaction.read(txString)

  lazy val relatedTxids: Set[ByteVector32] = {
    val rbfTxidSet = description.rbf.map(_.ofTxid).toSet
    rbfTxidSet ++ description.cpfpBy ++ description.cpfpOf + txid
  }
}

// USDT tx

object UsdtDescription {
  final val ETHEREUM = 1
  final val POLYGON = 2
}

case class UsdtDescription(fromAddrString: String, toAddrString: String, label: Option[String] = None) extends ItemDescription {
  def queryText(hash: String): String = hash + SEPARATOR + fromAddrString + SEPARATOR + toAddrString + label.getOrElse(new String)
  val semanticOrder: Option[SemanticOrder] = None
}

case class UsdtInfo(hashString: String, network: Int, block: Long, receivedUsdtString: String, sentUsdtString: String,
                    feeUsdtString: String, seenAt: Long, updatedAt: Long, description: UsdtDescription, balanceUsdt: Long,
                    incoming: Long, doubleSpent: Long) extends ItemDetails {
  override val isDoubleSpent: Boolean = 1L == doubleSpent
  override val identity: String = hashString
  val isIncoming: Boolean = 1L == incoming
}