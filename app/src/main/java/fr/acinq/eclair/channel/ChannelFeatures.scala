package fr.acinq.eclair.channel

import fr.acinq.eclair.Features.{
  AnchorOutputs,
  AnchorOutputsZeroFeeHtlcTx,
  ChannelType => ChannelTypeFeature,
  ResizeableHostedChannels,
  StaticRemoteKey
}
import fr.acinq.eclair.transactions.Transactions.{
  AnchorOutputsCommitmentFormat,
  CommitmentFormat,
  DefaultCommitmentFormat,
  ZeroFeeHtlcTxAnchorOutputsCommitmentFormat
}
import fr.acinq.eclair.{Feature, FeatureScope, Features, InitFeature}

case class ChannelFeatures(
                            activated: Set[Feature with FeatureScope] = Set.empty
                          ) {
  def hasFeature(feature: Feature with FeatureScope): Boolean =
    activated.contains(feature)

  lazy val paysDirectlyToWallet: Boolean = hasFeature(StaticRemoteKey)
  lazy val hostedResizeable: Boolean = hasFeature(ResizeableHostedChannels)
  lazy val channelType_opt: Option[SupportedChannelType] =
    if (hasFeature(ChannelTypeFeature) && hasFeature(AnchorOutputsZeroFeeHtlcTx))
      Some(ChannelTypes.AnchorOutputsZeroFeeHtlcTx)
    else if (hasFeature(ChannelTypeFeature) && hasFeature(AnchorOutputs))
      Some(ChannelTypes.AnchorOutputs)
    else if (hasFeature(ChannelTypeFeature) && hasFeature(StaticRemoteKey))
      Some(ChannelTypes.StaticRemoteKeyOnly)
    else None

  lazy val commitmentFormat: CommitmentFormat = channelType_opt match {
    case Some(ChannelTypes.AnchorOutputsZeroFeeHtlcTx) =>
      ZeroFeeHtlcTxAnchorOutputsCommitmentFormat
    case Some(ChannelTypes.AnchorOutputs) =>
      AnchorOutputsCommitmentFormat
    case Some(ChannelTypes.StaticRemoteKeyOnly) | None =>
      DefaultCommitmentFormat
  }
}

object ChannelFeatures {
  private def canUse(
                      localFeatures: Features[InitFeature],
                      remoteFeatures: Features[InitFeature],
                      feature: Feature with InitFeature
                    ): Boolean =
    Features.canUseFeature(localFeatures, remoteFeatures, feature)

  def apply(features: Feature with FeatureScope*): ChannelFeatures =
    ChannelFeatures(features.toSet)

  def fromChannelType(channelType: SupportedChannelType): ChannelFeatures =
    ChannelFeatures(
      ((channelType.features + ChannelTypeFeature).toSeq: Seq[Feature with FeatureScope]): _*
    )

  def pickChannelFeatures(
                           localFeatures: Features[InitFeature],
                           remoteFeatures: Features[InitFeature]
                         ): ChannelFeatures = {
    val withChannelType =
      canUse(localFeatures, remoteFeatures, ChannelTypeFeature)
    val zeroFeeAnchors =
      canUse(localFeatures, remoteFeatures, AnchorOutputsZeroFeeHtlcTx)
    val legacyAnchors = canUse(localFeatures, remoteFeatures, AnchorOutputs)
    val staticRemoteKey = canUse(localFeatures, remoteFeatures, StaticRemoteKey)

    if (withChannelType && staticRemoteKey && zeroFeeAnchors)
      fromChannelType(ChannelTypes.AnchorOutputsZeroFeeHtlcTx)
    else if (withChannelType && staticRemoteKey && legacyAnchors)
      fromChannelType(ChannelTypes.AnchorOutputs)
    else ChannelFeatures(StaticRemoteKey)
  }

  def pickChannelFeatures(
                           localFeatures: Features[InitFeature],
                           remoteFeatures: Features[InitFeature],
                           remoteChannelType_opt: Option[ChannelType]
                         ): ChannelFeatures = remoteChannelType_opt match {
    case Some(remoteType: SupportedChannelType)
      if canUse(localFeatures, remoteFeatures, ChannelTypeFeature) && remoteType.features
        .forall(canUse(localFeatures, remoteFeatures, _)) =>
      fromChannelType(remoteType)
    case None =>
      ChannelFeatures(StaticRemoteKey)
    case _ =>
      ChannelFeatures(StaticRemoteKey)
  }
}
