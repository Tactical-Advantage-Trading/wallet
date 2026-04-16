/*
 * Copyright 2019 ACINQ SAS
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package fr.acinq.eclair

import fr.acinq.eclair.FeatureSupport.{Mandatory, Optional}
import scodec.bits.{BitVector, ByteVector}


sealed trait FeatureSupport

// @formatter:off
object FeatureSupport {
  case object Mandatory extends FeatureSupport { override def toString: String = "mandatory" }
  case object Optional extends FeatureSupport { override def toString: String = "optional" }
}

trait Feature {

  this: FeatureScope =>

  def rfcName: String
  def mandatory: Int
  def optional: Int = mandatory + 1

  def supportBit(support: FeatureSupport): Int = support match {
    case Mandatory => mandatory
    case Optional => optional
  }

  override def toString: String = rfcName
}

/** Feature scope as defined in Bolt 9. */
sealed trait FeatureScope
/** Feature that should be advertised in init messages. */
trait InitFeature extends FeatureScope
/** Feature that should be advertised in node announcements. */
trait NodeFeature extends FeatureScope
/** Feature that should be advertised in invoices. */
trait InvoiceFeature extends FeatureScope
// @formatter:on

case class UnknownFeature(bitIndex: Int)

case class Features[T <: FeatureScope](activated: Map[Feature with T, FeatureSupport], unknown: Set[UnknownFeature] = Set.empty) {

  def hasFeature(feature: Feature with T, support: Option[FeatureSupport] = None): Boolean = support match {
    case Some(s) => activated.get(feature).contains(s)
    case None => activated.contains(feature)
  }

  def hasPluginFeature(feature: UnknownFeature): Boolean = unknown.contains(feature)

  /** NB: this method is not reflexive, see [[Features.areCompatible]] if you want symmetric validation. */
  def areSupported(remoteFeatures: Features[T]): Boolean = {
    // we allow unknown odd features (it's ok to be odd)
    val unknownFeaturesOk = remoteFeatures.unknown.forall(_.bitIndex % 2 == 1)
    // we verify that we activated every mandatory feature they require
    val knownFeaturesOk = remoteFeatures.activated.forall {
      case (_, Optional) => true
      case (feature, Mandatory) => hasFeature(feature)
    }
    unknownFeaturesOk && knownFeaturesOk
  }

  def initFeatures(): Features[InitFeature] = Features[InitFeature](activated.collect { case (f: InitFeature, s) => (f, s) }.toSeq:_*)

  def nodeAnnouncementFeatures(): Features[NodeFeature] = Features[NodeFeature](activated.collect { case (f: NodeFeature, s) => (f, s) }.toSeq:_*)

  def invoiceFeatures(): Features[InvoiceFeature] = Features[InvoiceFeature](activated.collect { case (f: InvoiceFeature, s) => (f, s) }.toSeq:_*)

  def unscoped(): Features[FeatureScope] = Features[FeatureScope](activated.collect { case (f, s) => (f: Feature with FeatureScope, s) }, unknown)

  def toByteVector: ByteVector = {
    val activatedFeatureBytes = toByteVectorFromIndex(activated.map { case (feature, support) => feature.supportBit(support) }.toSet)
    val unknownFeatureBytes = toByteVectorFromIndex(unknown.map(_.bitIndex))
    val maxSize = activatedFeatureBytes.size.max(unknownFeatureBytes.size)
    activatedFeatureBytes.padLeft(maxSize) | unknownFeatureBytes.padLeft(maxSize)
  }

  private def toByteVectorFromIndex(indexes: Set[Int]): ByteVector = {
    if (indexes.isEmpty) return ByteVector.empty
    // When converting from BitVector to ByteVector, scodec pads right instead of left, so we make sure we pad to bytes *before* setting feature bits.
    var buf = BitVector.fill(indexes.max + 1)(high = false).bytes.bits
    indexes.foreach { i => buf = buf.set(i) }
    buf.reverse.bytes
  }

  override def toString: String = {
    val a = activated.map { case (feature, support) => feature.rfcName + ":" + support }.mkString(",")
    val u = unknown.map(_.bitIndex).mkString(",")
    s"$a" + (if (unknown.nonEmpty) s" (unknown=$u)" else "")
  }
}

object Features {

  def empty[T <: FeatureScope]: Features[T] = Features[T](Map.empty[Feature with T, FeatureSupport])

  def apply[T <: FeatureScope](features: (Feature with T, FeatureSupport)*): Features[T] = Features[T](features.toMap)

  def apply(bytes: ByteVector): Features[FeatureScope] = apply(bytes.bits)

  def apply(bits: BitVector): Features[FeatureScope] = {
    val all = bits.toIndexedSeq.reverse.zipWithIndex.collect {
      case (true, idx) if knownFeatures.exists(_.optional == idx) => Right((knownFeatures.find(_.optional == idx).get, Optional))
      case (true, idx) if knownFeatures.exists(_.mandatory == idx) => Right((knownFeatures.find(_.mandatory == idx).get, Mandatory))
      case (true, idx) => Left(UnknownFeature(idx))
    }
    Features[FeatureScope](
      activated = all.collect { case Right((feature, support)) => feature -> support }.toMap,
      unknown = all.collect { case Left(inf) => inf }.toSet
    )
  }

  case object DataLossProtect extends Feature with InitFeature with NodeFeature {
    val rfcName = "Data loss protect"
    val mandatory = 0
  }

  case object InitialRoutingSync extends Feature with InitFeature {
    val rfcName = "Initial routing sync"
    val mandatory = 2
  }

  case object ChannelRangeQueries extends Feature with InitFeature with NodeFeature {
    val rfcName = "Basic gossip queries"
    val mandatory = 6
  }

  case object VariableLengthOnion extends Feature with InitFeature with NodeFeature with InvoiceFeature {
    val rfcName = "Advanced onion"
    val mandatory = 8
  }

  case object ChannelRangeQueriesExtended extends Feature with InitFeature with NodeFeature {
    val rfcName = "Fast graph sync"
    val mandatory = 10
  }

  case object StaticRemoteKey extends Feature with InitFeature with NodeFeature {
    val rfcName = "Direct refund"
    val mandatory = 12
  }

  case object PaymentSecret extends Feature with InitFeature with NodeFeature with InvoiceFeature {
    val rfcName = "Payment secret"
    val mandatory = 14
  }

  case object BasicMultiPartPayment extends Feature with InitFeature with NodeFeature with InvoiceFeature {
    val rfcName = "Multipart payments"
    val mandatory = 16
  }

  case object Wumbo extends Feature with InitFeature with NodeFeature {
    val rfcName = "Large channels"
    val mandatory = 18
  }

  case object AnchorOutputs
    extends Feature
      with InitFeature
      with NodeFeature {
    val rfcName = "Anchor outputs (legacy)"
    val mandatory = 20
  }

  case object AnchorOutputsZeroFeeHtlcTx
    extends Feature
      with InitFeature
      with NodeFeature {
    val rfcName = "Anchor outputs zero-fee htlc tx"
    val mandatory = 22
  }

  case object ShutdownAnySegwit extends Feature with InitFeature with NodeFeature {
    val rfcName = "Any shutdown script"
    val mandatory = 26
  }

  case object ChannelType extends Feature with InitFeature with NodeFeature {
    val rfcName = "Channel type"
    val mandatory = 44
  }

  case object ScidAlias extends Feature with InitFeature with NodeFeature {
    val rfcName = "SCID alias"
    val mandatory = 46
  }

  case object OnionMessages extends Feature with InitFeature with NodeFeature {
    val rfcName = "Onion messages"
    val mandatory = 38
  }

  case object PaymentMetadata extends Feature with InvoiceFeature {
    val rfcName = "Payment invoice metadata"
    val mandatory = 48
  }

  case object TrampolinePayment extends Feature with InitFeature with NodeFeature with InvoiceFeature {
    val rfcName = "Trampoline payments"
    val mandatory = 50
  }

  case object ChainSwap extends Feature with InitFeature with NodeFeature {
    val rfcName = "Chain swaps"
    val mandatory = 32770
  }

  case object HostedChannels extends Feature with InitFeature with NodeFeature {
    val rfcName = "Hosted channels"
    val mandatory = 52972
  }

  case object ResizeableHostedChannels extends Feature with InitFeature with NodeFeature {
    val rfcName = "Resizeable Hosted channels"
    val mandatory = 52974
  }

  val knownFeatures: Set[Feature with FeatureScope] = Set(
    DataLossProtect,
    InitialRoutingSync,
    ChannelRangeQueries,
    VariableLengthOnion,
    ChannelRangeQueriesExtended,
    PaymentSecret,
    BasicMultiPartPayment,
    Wumbo,
    AnchorOutputs,
    AnchorOutputsZeroFeeHtlcTx,
    StaticRemoteKey,
    ShutdownAnySegwit,
    OnionMessages,
    ChannelType,
    ScidAlias,
    PaymentMetadata,
    TrampolinePayment,
    ChainSwap,
    HostedChannels,
    ResizeableHostedChannels
  )

  case class FeatureException(message: String) extends IllegalArgumentException(message)

  /** Returns true if both feature sets are compatible. */
  def areCompatible[T <: FeatureScope](ours: Features[T], theirs: Features[T]): Boolean = ours.areSupported(theirs) && theirs.areSupported(ours)

  /** returns true if both have at least optional support */
  def canUseFeature[T <: FeatureScope](localFeatures: Features[T], remoteFeatures: Features[T], feature: Feature with T): Boolean = {
    localFeatures.hasFeature(feature) && remoteFeatures.hasFeature(feature)
  }
}
