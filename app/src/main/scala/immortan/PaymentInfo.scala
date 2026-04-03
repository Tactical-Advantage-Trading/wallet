package immortan

import fr.acinq.bitcoin.{ByteVector32, Satoshi, Transaction}
import Tools.{Any2Some, ExtPubKeys, SEPARATOR, StringList}
import immortan.utils.ImplicitJsonFormats._
import fr.acinq.eclair._
import java.util.Date


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
  val taRoi: Option[BigDecimal]
  val semanticOrder: Option[SemanticOrder]
  val label: Option[String]
  val networkId: Int
}

object CoinDescription {
  final val RBF_CANCEL = 1
  final val RBF_BOOST = 2
}

case class CoinDescription(addresses: StringList, label: Option[String], networkId: Int, semanticOrder: Option[SemanticOrder] = None,
                           cpfpBy: Option[ByteVector32] = None, cpfpOf: Option[ByteVector32] = None, rbf: Option[RBFParams] = None,
                           taRoi: Option[BigDecimal] = None) extends ItemDescription {
  def queryText(txid: ByteVector32): String = txid.toHex + SEPARATOR + addresses.mkString(SEPARATOR) + SEPARATOR + label.getOrElse(new String)
  def withNewOrderCond(order: Option[SemanticOrder] = None): CoinDescription = if (semanticOrder.isDefined) this else copy(semanticOrder = order)
  def withNewCPFPBy(txid: ByteVector32): CoinDescription = copy(cpfpBy = txid.asSome)
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

case class CoinDetails(txString: String, identity: String, extPubsString: String, depth: Long, receivedSat: Satoshi, sentSat: Satoshi,
                       feeSat: Satoshi, seenAt: Long, updatedAt: Long, description: CoinDescription, balanceSnapshot: MilliSatoshi,
                       fiatRatesString: String, incoming: Long, doubleSpent: Long) extends ItemDetails {
  override val isDoubleSpent: Boolean = 1L == doubleSpent
  val isIncoming: Boolean = 1L == incoming
  val isConfirmed: Boolean = depth > 0

  lazy val extPubs: ExtPubKeys = tryTo[ExtPubKeys](extPubsString).getOrElse(Nil)
  lazy val txid: ByteVector32 = ByteVector32.fromValidHex(identity)
  lazy val tx: Transaction = Transaction.read(txString)

  lazy val relatedTxids: Set[String] = {
    val rbfs = description.rbf.map(_.ofTxid).toSet
    val cpfps = rbfs ++ description.cpfpBy ++ description.cpfpOf
    cpfps.map(_.toHex) + identity
  }
}
