package fr.acinq.eclair.wire

import fr.acinq.eclair.channel.{ChannelType, ChannelTypes}
import fr.acinq.eclair.Features
import fr.acinq.eclair.UInt64
import fr.acinq.eclair.wire.CommonCodecs._
import fr.acinq.eclair.wire.TlvCodecs.tlvStream
import scodec.Codec
import scodec.bits.ByteVector
import scodec.codecs._


sealed trait OpenChannelTlv extends Tlv
sealed trait AcceptChannelTlv extends Tlv

object ChannelTlv {

  /** Commitment to where the funds will go in case of a mutual close, which remote node will enforce in case we're compromised. */
  case class UpfrontShutdownScript(script: ByteVector) extends OpenChannelTlv with AcceptChannelTlv {
    val isEmpty: Boolean = script.isEmpty
  }

  /** A channel type is a set of even feature bits that represent persistent
   * channel features.
   */
  case class ChannelTypeTlv(channelType: ChannelType)
    extends OpenChannelTlv
      with AcceptChannelTlv

  val channelTypeCodec: Codec[ChannelTypeTlv] =
    variableSizeBytesLong(varintoverflow, bytes).xmap[ChannelTypeTlv](
      b =>
        ChannelTypeTlv(
          ChannelTypes.fromFeatures(Features(b).initFeatures())
        ),
      tlv => tlv.channelType.featureBits.toByteVector
    )
}

object OpenChannelTlv {

  import ChannelTlv._

  val openTlvCodec: Codec[TlvStream[OpenChannelTlv]] = tlvStream(
    discriminated[OpenChannelTlv]
      .by(varint)
      .\(UInt64(0)) { case v: UpfrontShutdownScript => v }(
        variableSizeBytesLong(varintoverflow, bytes).as[UpfrontShutdownScript]
      )
      .\(UInt64(1)) { case v: ChannelTypeTlv => v }(channelTypeCodec)
  )

}

object AcceptChannelTlv {

  import ChannelTlv._

  val acceptTlvCodec: Codec[TlvStream[AcceptChannelTlv]] = tlvStream(
    discriminated[AcceptChannelTlv]
      .by(varint)
      .\(UInt64(0)) { case v: UpfrontShutdownScript => v }(
        variableSizeBytesLong(varintoverflow, bytes).as[UpfrontShutdownScript]
      )
      .\(UInt64(1)) { case v: ChannelTypeTlv => v }(channelTypeCodec)
  )
}

// channel_ready (FundingLocked) TLVs
sealed trait FundingLockedTlv extends Tlv

object FundingLockedTlv {
  /** Alias SCID that the peer should use when referring to this channel (option_scid_alias, type 1). */
  case class ShortChannelIdAlias(alias: Long) extends FundingLockedTlv

  val fundingLockedTlvCodec: Codec[TlvStream[FundingLockedTlv]] = tlvStream(
    discriminated[FundingLockedTlv]
      .by(varint)
      .\(UInt64(1)) { case v: ShortChannelIdAlias => v }(
        variableSizeBytesLong(varintoverflow, int64).as[ShortChannelIdAlias]
      )
  )
}