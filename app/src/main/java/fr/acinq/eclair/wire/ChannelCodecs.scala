package fr.acinq.eclair.wire

import fr.acinq.bitcoin.DeterministicWallet.{ExtendedPrivateKey, ExtendedPublicKey, KeyPath}
import fr.acinq.bitcoin.{OutPoint, Transaction, TxOut}
import fr.acinq.eclair._
import fr.acinq.eclair.blockchain.TxConfirmedAt
import fr.acinq.eclair.channel._
import fr.acinq.eclair.crypto.ShaChain
import fr.acinq.eclair.transactions.Transactions._
import fr.acinq.eclair.transactions._
import fr.acinq.eclair.wire.CommonCodecs._
import fr.acinq.eclair.wire.LightningMessageCodecs._
import fr.acinq.eclair.{FeatureSupport, Features, MilliSatoshi}
import immortan.{HostedCommits, RemoteNodeInfo}
import scodec.bits.ByteVector
import scodec.codecs._
import scodec.{Attempt, Codec}


object ChannelCodecs {
  // All LN protocol message must be stored as length-delimited, because they may have arbitrary trailing data
  def lengthDelimited[T](codec: Codec[T]): Codec[T] = variableSizeBytesLong(varintoverflow, codec)

  val keyPathCodec = (listOfN(uint16, uint32) withContext "path").xmap[KeyPath](KeyPath.apply, _.path.toList).as[KeyPath]

  val extendedPrivateKeyCodec = {
    ("secretkeybytes" | bytes32) ::
      ("chaincode" | bytes32) ::
      ("depth" | uint16) ::
      ("path" | keyPathCodec) ::
      ("parent" | int64)
  }.as[ExtendedPrivateKey]

  val extendedPublicKeyCodec = {
    ("publickeybytes" | varsizebinarydata) ::
      ("chaincode" | bytes32) ::
      ("depth" | uint16) ::
      ("path" | keyPathCodec) ::
      ("parent" | int64)
  }.as[ExtendedPublicKey]

  val outPointCodec = lengthDelimited(bytes.xmap(d => OutPoint.read(d.toArray), OutPoint.write))

  val txOutCodec = lengthDelimited(bytes.xmap(d => TxOut.read(d.toArray), TxOut.write))

  val txCodec = lengthDelimited(bytes.xmap(d => Transaction.read(d.toArray), Transaction.write))

  /**
   * byte-aligned boolean codec
   */
  val bool8: Codec[Boolean] = bool(8)

  val htlcCodec =
    discriminated[DirectedHtlc].by(bool8)
      .typecase(true, lengthDelimited(updateAddHtlcCodec).as[IncomingHtlc])
      .typecase(false, lengthDelimited(updateAddHtlcCodec).as[OutgoingHtlc])

  val commitmentSpecCodec = {
    ("feeratePerKw" | feeratePerKw) ::
      ("toLocal" | millisatoshi) ::
      ("toRemote" | millisatoshi) ::
      ("htlcs" | setCodec(htlcCodec))
  }.as[CommitmentSpec]

  val inputInfoCodec = {
    ("outPoint" | outPointCodec) ::
      ("txOut" | txOutCodec) ::
      ("redeemScript" | lengthDelimited(bytes))
  }.as[InputInfo]

  val commitTxCodec = {
    ("inputInfo" | inputInfoCodec) ::
      ("tx" | txCodec)
  }.as[CommitTx]

  val txWithInputInfoCodec =
    discriminated[TransactionWithInputInfo].by(uint16)
      .typecase(0x01, commitTxCodec)
      .typecase(0x02, (("inputInfo" | inputInfoCodec) :: ("tx" | txCodec) :: ("paymentHash" | bytes32)).as[HtlcSuccessTx])
      .typecase(0x03, (("inputInfo" | inputInfoCodec) :: ("tx" | txCodec)).as[HtlcTimeoutTx])
      .typecase(0x04, (("inputInfo" | inputInfoCodec) :: ("tx" | txCodec)).as[ClaimHtlcSuccessTx])
      .typecase(0x05, (("inputInfo" | inputInfoCodec) :: ("tx" | txCodec)).as[ClaimHtlcTimeoutTx])
      .typecase(0x06, (("inputInfo" | inputInfoCodec) :: ("tx" | txCodec)).as[ClaimP2WPKHOutputTx])
      .typecase(0x07, (("inputInfo" | inputInfoCodec) :: ("tx" | txCodec)).as[ClaimLocalDelayedOutputTx])
      .typecase(0x08, (("inputInfo" | inputInfoCodec) :: ("tx" | txCodec)).as[MainPenaltyTx])
      .typecase(0x09, (("inputInfo" | inputInfoCodec) :: ("tx" | txCodec)).as[HtlcPenaltyTx])
      .typecase(0x10, (("inputInfo" | inputInfoCodec) :: ("tx" | txCodec)).as[ClosingTx])

  val htlcTxAndSigsCodec = {
    ("txinfo" | txWithInputInfoCodec) ::
      ("localSig" | lengthDelimited(bytes64)) ::
      ("remoteSig" | lengthDelimited(bytes64))
  }.as[HtlcTxAndSigs]

  val publishableTxsCodec = {
    ("commitTx" | commitTxCodec) ::
      ("htlcTxsAndSigs" | listOfN(uint16, htlcTxAndSigsCodec))
  }.as[PublishableTxs]

  val localCommitCodec = {
    ("index" | uint64overflow) ::
      ("spec" | commitmentSpecCodec) ::
      ("publishableTxs" | publishableTxsCodec)
  }.as[LocalCommit]

  val remoteCommitCodec = {
    ("index" | uint64overflow) ::
      ("spec" | commitmentSpecCodec) ::
      ("txid" | bytes32) ::
      ("remotePerCommitmentPoint" | publicKey)
  }.as[RemoteCommit]

  val updateMessageCodec = lengthDelimited {
    lightningMessageCodec.narrow[UpdateMessage](Attempt successful _.asInstanceOf[UpdateMessage], identity)
  }

  val localChangesCodec = {
    ("proposed" | listOfN(uint16, updateMessageCodec)) ::
      ("signed" | listOfN(uint16, updateMessageCodec)) ::
      ("acked" | listOfN(uint16, updateMessageCodec))
  }.as[LocalChanges]

  val remoteChangesCodec = {
    ("proposed" | listOfN(uint16, updateMessageCodec)) ::
      ("acked" | listOfN(uint16, updateMessageCodec)) ::
      ("signed" | listOfN(uint16, updateMessageCodec))
  }.as[RemoteChanges]

  val waitingForRevocationCodec = {
    ("nextRemoteCommit" | remoteCommitCodec) ::
      ("sent" | lengthDelimited(commitSigCodec)) ::
      ("sentAfterLocalCommitIndex" | uint64overflow)
  }.as[WaitingForRevocation]

  val channelKeysCodec = {
    ("path" | keyPathCodec) ::
      ("shaSeed" | bytes32) ::
      ("fundingKey" | extendedPrivateKeyCodec) ::
      ("revocationKey" | extendedPrivateKeyCodec) ::
      ("paymentKey" | extendedPrivateKeyCodec) ::
      ("delayedPaymentKey" | extendedPrivateKeyCodec) ::
      ("htlcKey" | extendedPrivateKeyCodec)
  }.as[ChannelKeys]

  val channelFeaturesCodec: Codec[ChannelFeatures] = lengthDelimited(bytes).xmap(
    (b: ByteVector) => ChannelFeatures(Features(b).activated.keySet), // we make no difference between mandatory/optional, both are considered activated
    (cf: ChannelFeatures) => Features(cf.activated.map(f => f -> FeatureSupport.Mandatory).toMap).toByteVector // we encode features as mandatory, by convention
  )

  val localParamsCodec = {
    ("keys" | channelKeysCodec) ::
      ("dustLimit" | satoshi) ::
      ("maxHtlcValueInFlightMsat" | uint64) ::
      ("channelReserve" | satoshi) ::
      ("htlcMinimum" | millisatoshi) ::
      ("toSelfDelay" | cltvExpiryDelta) ::
      ("maxAcceptedHtlcs" | uint16) ::
      ("isFunder" | bool8) ::
      ("defaultFinalScriptPubKey" | lengthDelimited(bytes)) ::
      ("walletStaticPaymentBasepoint" | publicKey)
  }.as[LocalParams]

  val remoteParamsCodec = {
    (satoshi withContext "dustLimit") ::
      (uint64 withContext "maxHtlcValueInFlightMsat") ::
      (satoshi withContext "channelReserve") ::
      (millisatoshi withContext "htlcMinimum") ::
      (cltvExpiryDelta withContext "toSelfDelay") ::
      (uint16 withContext "maxAcceptedHtlcs") ::
      (publicKey withContext "fundingPubKey") ::
      (publicKey withContext "revocationBasepoint") ::
      (publicKey withContext "paymentBasepoint") ::
      (publicKey withContext "delayedPaymentBasepoint") ::
      (publicKey withContext "htlcBasepoint") ::
      (optional(bool8, lengthDelimited(bytes)) withContext "shutdownScript")
  }.as[RemoteParams]

  val remoteNodeInfoCodec = {
    (publicKey withContext "nodeId") ::
      (nodeaddress withContext "address") ::
      (zeropaddedstring(32) withContext "alias")
  }.as[RemoteNodeInfo]

  val channelLabelCodec = (text withContext "label").as[ChannelLabel]

  val extParamsCodec = discriminated[ExtParams].by(uint16)
    .typecase(1, channelLabelCodec)
    .typecase(2, (int64 withContext "alias").as[RemoteAlias])

  val commitmentsCodec = {
    (byte withContext "channelFlags") ::
      (bytes32 withContext "channelId") ::
      (channelFeaturesCodec withContext "channelFeatures") ::
      (either(bool8, waitingForRevocationCodec, publicKey) withContext "remoteNextCommitInfo") ::
      (byteAligned(ShaChain.shaChainCodec) withContext "remotePerCommitmentSecrets") ::
      (optional(bool8, lengthDelimited(channelUpdateCodec)) withContext "updateOpt") ::
      (setCodec(uint64overflow) withContext "postCloseOutgoingResolvedIds") ::
      (remoteNodeInfoCodec withContext "remoteInfo") ::
      (localParamsCodec withContext "localParams") ::
      (remoteParamsCodec withContext "remoteParams") ::
      (localCommitCodec withContext "localCommit") ::
      (remoteCommitCodec withContext "remoteCommit") ::
      (localChangesCodec withContext "localChanges") ::
      (remoteChangesCodec withContext "remoteChanges") ::
      (uint64overflow withContext "localNextHtlcId") ::
      (uint64overflow withContext "remoteNextHtlcId") ::
      (inputInfoCodec withContext "commitInput") ::
      (listOfN(uint16, extParamsCodec) withContext "extParams") ::
      (int64 withContext "startedAt")
  }.as[NormalCommits]

  val closingTxProposedCodec = {
    (txCodec withContext "unsignedTx") ::
      (lengthDelimited(closingSignedCodec) withContext "localClosingSigned")
  }.as[ClosingTxProposed]

  val txConfirmedAtCodec = {
    (uint24 withContext "blockHeight") ::
      (txCodec withContext "tx")
  }.as[TxConfirmedAt]

  val spentMapCodec = mapCodec(outPointCodec, txConfirmedAtCodec)

  val localCommitPublishedCodec = {
    (txCodec withContext "commitTx") ::
      (optional(bool8, txCodec) withContext "claimMainDelayedOutputTx") ::
      (listOfN(uint16, txCodec) withContext "htlcSuccessTxs") ::
      (listOfN(uint16, txCodec) withContext "htlcTimeoutTxs") ::
      (listOfN(uint16, txCodec) withContext "claimHtlcDelayedTx") ::
      (spentMapCodec withContext "irrevocablySpent")
  }.as[LocalCommitPublished]

  val remoteCommitPublishedCodec = {
    (txCodec withContext "commitTx") ::
      (optional(bool8, txCodec) withContext "claimMainOutputTx") ::
      (listOfN(uint16, txCodec) withContext "claimHtlcSuccessTxs") ::
      (listOfN(uint16, txCodec) withContext "claimHtlcTimeoutTxs") ::
      (spentMapCodec withContext "irrevocablySpent")
  }.as[RemoteCommitPublished]

  val revokedCommitPublishedCodec = {
    (txCodec withContext "commitTx") ::
      (optional(bool8, txCodec) withContext "claimMainOutputTx") ::
      (optional(bool8, txCodec) withContext "mainPenaltyTx") ::
      (listOfN(uint16, txCodec) withContext "htlcPenaltyTxs") ::
      (listOfN(uint16, txCodec) withContext "claimHtlcDelayedPenaltyTxs") ::
      (spentMapCodec withContext "irrevocablySpent")
  }.as[RevokedCommitPublished]

  val DATA_WAIT_FOR_FUNDING_CONFIRMED_Codec = {
    (commitmentsCodec withContext "commitments") ::
      (optional(bool8, txCodec) withContext "fundingTx") ::
      (int64 withContext "waitingSince") ::
      (either(bool8, lengthDelimited(fundingCreatedCodec), lengthDelimited(fundingSignedCodec)) withContext "lastSent") ::
      (optional(bool8, lengthDelimited(fundingLockedCodec)) withContext "deferred")
  }.as[DATA_WAIT_FOR_FUNDING_CONFIRMED]

  val DATA_WAIT_FOR_FUNDING_LOCKED_Codec = {
    (commitmentsCodec withContext "commitments") ::
      (int64 withContext "shortChannelId") ::
      (lengthDelimited(fundingLockedCodec) withContext "lastSent")
  }.as[DATA_WAIT_FOR_FUNDING_LOCKED]

  val DATA_NORMAL_Codec = {
    (commitmentsCodec withContext "commitments") ::
      (int64 withContext "shortChannelId") ::
      (bool8 withContext "feeUpdateRequired") ::
      (listOfN(uint16, varsizebinarydata) withContext "extParams") ::
      (optional(bool8, lengthDelimited(shutdownCodec)) withContext "localShutdown") ::
      (optional(bool8, lengthDelimited(shutdownCodec)) withContext "remoteShutdown")
  }.as[DATA_NORMAL]

  val DATA_NEGOTIATING_Codec = {
    (commitmentsCodec withContext "commitments") ::
      (lengthDelimited(shutdownCodec) withContext "localShutdown") ::
      (lengthDelimited(shutdownCodec) withContext "remoteShutdown") ::
      (listOfN(uint16, listOfN(uint16, lengthDelimited(closingTxProposedCodec))) withContext "closingTxProposed") ::
      (optional(bool8, txCodec) withContext "bestUnpublishedClosingTxOpt")
  }.as[DATA_NEGOTIATING]

  val DATA_CLOSING_Codec = {
    (commitmentsCodec withContext "commitments") ::
      (int64 withContext "waitingSince") ::
      (listOfN(uint16, txCodec) withContext "mutualCloseProposed") ::
      (listOfN(uint16, txCodec) withContext "mutualClosePublished") ::
      (optional(bool8, localCommitPublishedCodec) withContext "localCommitPublished") ::
      (optional(bool8, remoteCommitPublishedCodec) withContext "remoteCommitPublished") ::
      (optional(bool8, remoteCommitPublishedCodec) withContext "nextRemoteCommitPublished") ::
      (optional(bool8, remoteCommitPublishedCodec) withContext "futureRemoteCommitPublished") ::
      (listOfN(uint16, revokedCommitPublishedCodec) withContext "revokedCommitPublished")
  }.as[DATA_CLOSING]

  val DATA_WAIT_FOR_REMOTE_PUBLISH_FUTURE_COMMITMENT_Codec = {
    (commitmentsCodec withContext "commitments") ::
      (channelReestablishCodec withContext "remoteChannelReestablish")
  }.as[DATA_WAIT_FOR_REMOTE_PUBLISH_FUTURE_COMMITMENT]

  val hostedCommitsCodec = {
    (remoteNodeInfoCodec withContext "remoteInfo") ::
      (commitmentSpecCodec withContext "localSpec") ::
      (lengthDelimited(lastCrossSignedStateCodec) withContext "lastCrossSignedState") ::
      (listOfN(uint16, updateMessageCodec) withContext "nextLocalUpdates") ::
      (listOfN(uint16, updateMessageCodec) withContext "nextRemoteUpdates") ::
      (optional(bool8, lengthDelimited(channelUpdateCodec)) withContext "updateOpt") ::
      (setCodec(uint64overflow) withContext "postErrorOutgoingResolvedIds") ::
      (optional(bool8, lengthDelimited(failCodec)) withContext "localError") ::
      (optional(bool8, lengthDelimited(failCodec)) withContext "remoteError") ::
      (optional(bool8, lengthDelimited(resizeChannelCodec)) withContext "resizeProposal") ::
      (optional(bool8, lengthDelimited(marginChannelCodec)) withContext "marginProposal") ::
      (optional(bool8, lengthDelimited(stateOverrideCodec)) withContext "overrideProposal") ::
      (listOfN(uint16, extParamsCodec) withContext "extParams") ::
      (int64 withContext "startedAt") ::
      (millisatoshi withContext "currentHostRate")
  }.as[HostedCommits]

  val persistentDataCodec =
    discriminated[PersistentChannelData].by(uint16)
      .typecase(1, DATA_WAIT_FOR_FUNDING_CONFIRMED_Codec)
      .typecase(2, DATA_WAIT_FOR_FUNDING_LOCKED_Codec)
      .typecase(3, DATA_NORMAL_Codec)
      .typecase(4, DATA_NEGOTIATING_Codec)
      .typecase(5, DATA_CLOSING_Codec)
      .typecase(6, DATA_WAIT_FOR_REMOTE_PUBLISH_FUTURE_COMMITMENT_Codec)
      .typecase(7, hostedCommitsCodec)
}
