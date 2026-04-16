package fr.acinq.eclair.wire

import fr.acinq.eclair._
import fr.acinq.eclair.wire.ChannelCodecs._
import fr.acinq.eclair.wire.CommonCodecs._
import immortan.Ticker
import scodec.{Attempt, Codec, DecodeResult, Err}
import scodec.bits.{BitVector, ByteVector}
import scodec.codecs._


object LightningMessageCodecs {

  val featuresCodec: Codec[Features[FeatureScope]] = varsizebinarydata.xmap[Features[FeatureScope]](
    { bytes => Features(bytes) },
    { features => features.toByteVector }
  )

  val initFeaturesCodec: Codec[Features[InitFeature]] = featuresCodec.xmap[Features[InitFeature]](_.initFeatures, _.unscoped)

  /** For historical reasons, features are divided into two feature bitmasks. We only send from the second one, but we allow receiving in both. */
  val combinedFeaturesCodec: Codec[Features[InitFeature]] = (
    ("globalFeatures" | varsizebinarydata) ::
      ("localFeatures" | varsizebinarydata)).as[(ByteVector, ByteVector)].xmap[Features[InitFeature]](
    { case (gf, lf) =>
      val length = gf.length.max(lf.length)
      Features(gf.padLeft(length) | lf.padLeft(length)).initFeatures()
    },
    { features => (ByteVector.empty, features.toByteVector) })

  val initCodec = (("features" | combinedFeaturesCodec) :: ("tlvStream" | InitTlvCodecs.initTlvCodec)).as[Init]

  val failCodec = (("channelId" | bytes32) :: ("data" | varsizebinarydata)).as[Fail]

  val warningCodec = (("channelId" | bytes32) :: ("data" | varsizebinarydata)).as[Warning]

  val pingCodec = (("pongLength" | uint16) :: ("data" | varsizebinarydata)).as[Ping]

  val pongCodec = ("data" | varsizebinarydata).as[Pong]

  val channelReestablishCodec = {
    ("channelId" | bytes32) ::
      ("nextLocalCommitmentNumber" | uint64overflow) ::
      ("nextRemoteRevocationNumber" | uint64overflow) ::
      ("yourLastPerCommitmentSecret" | privateKey) ::
      ("myCurrentPerCommitmentPoint" | publicKey)
  }.as[ChannelReestablish]

  val openChannelCodec = {
    ("chainHash" | bytes32) ::
      ("temporaryChannelId" | bytes32) ::
      ("fundingSatoshis" | satoshi) ::
      ("pushMsat" | millisatoshi) ::
      ("dustLimitSatoshis" | satoshi) ::
      ("maxHtlcValueInFlightMsat" | uint64) ::
      ("channelReserveSatoshis" | satoshi) ::
      ("htlcMinimumMsat" | millisatoshi) ::
      ("feeratePerKw" | feeratePerKw) ::
      ("toSelfDelay" | cltvExpiryDelta) ::
      ("maxAcceptedHtlcs" | uint16) ::
      ("fundingPubkey" | publicKey) ::
      ("revocationBasepoint" | publicKey) ::
      ("paymentBasepoint" | publicKey) ::
      ("delayedPaymentBasepoint" | publicKey) ::
      ("htlcBasepoint" | publicKey) ::
      ("firstPerCommitmentPoint" | publicKey) ::
      ("channelFlags" | byte) ::
      ("tlvStream" | OpenChannelTlv.openTlvCodec)
  }.as[OpenChannel]

  val acceptChannelCodec = {
    ("temporaryChannelId" | bytes32) ::
      ("dustLimitSatoshis" | satoshi) ::
      ("maxHtlcValueInFlightMsat" | uint64) ::
      ("channelReserveSatoshis" | satoshi) ::
      ("htlcMinimumMsat" | millisatoshi) ::
      ("minimumDepth" | uint32) ::
      ("toSelfDelay" | cltvExpiryDelta) ::
      ("maxAcceptedHtlcs" | uint16) ::
      ("fundingPubkey" | publicKey) ::
      ("revocationBasepoint" | publicKey) ::
      ("paymentBasepoint" | publicKey) ::
      ("delayedPaymentBasepoint" | publicKey) ::
      ("htlcBasepoint" | publicKey) ::
      ("firstPerCommitmentPoint" | publicKey) ::
      ("tlvStream" | AcceptChannelTlv.acceptTlvCodec)
  }.as[AcceptChannel]

  val fundingCreatedCodec = {
    ("temporaryChannelId" | bytes32) ::
      ("fundingTxid" | bytes32) ::
      ("fundingOutputIndex" | uint16) ::
      ("signature" | bytes64)
  }.as[FundingCreated]

  val fundingSignedCodec = {
    ("channelId" | bytes32) ::
      ("signature" | bytes64)
  }.as[FundingSigned]

  val fundingLockedCodec = {
    ("channelId" | bytes32) ::
      ("nextPerCommitmentPoint" | publicKey) ::
      ("tlvStream" | FundingLockedTlv.fundingLockedTlvCodec)
  }.as[FundingLocked]

  val shutdownCodec = {
    ("channelId" | bytes32) ::
      ("scriptPubKey" | varsizebinarydata)
  }.as[Shutdown]

  val closingSignedCodec = {
    ("channelId" | bytes32) ::
      ("feeSatoshis" | satoshi) ::
      ("signature" | bytes64)
  }.as[ClosingSigned]

  val updateAddHtlcCodec = {
    ("channelId" | bytes32) ::
      ("id" | uint64overflow) ::
      ("amountMsat" | millisatoshi) ::
      ("paymentHash" | bytes32) ::
      ("expiry" | cltvExpiry) ::
      ("onionRoutingPacket" | PaymentOnionCodecs.paymentOnionPacketCodec) ::
      ("tlvStream" | PaymentTagTlv.codec)
  }.as[UpdateAddHtlc]

  val updateFulfillHtlcCodec = {
    ("channelId" | bytes32) ::
      ("id" | uint64overflow) ::
      ("paymentPreimage" | bytes32)
  }.as[UpdateFulfillHtlc]

  val updateFailHtlcCodec = {
    ("channelId" | bytes32) ::
      ("id" | uint64overflow) ::
      ("reason" | varsizebinarydata)
  }.as[UpdateFailHtlc]

  val updateFailMalformedHtlcCodec = {
    ("channelId" | bytes32) ::
      ("id" | uint64overflow) ::
      ("onionHash" | bytes32) ::
      ("failureCode" | uint16)
  }.as[UpdateFailMalformedHtlc]

  val commitSigCodec = {
    ("channelId" | bytes32) ::
      ("signature" | bytes64) ::
      ("htlcSignatures" | listofsignatures)
  }.as[CommitSig]

  val revokeAndAckCodec = {
    ("channelId" | bytes32) ::
      ("perCommitmentSecret" | privateKey) ::
      ("nextPerCommitmentPoint" | publicKey)
  }.as[RevokeAndAck]

  val updateFeeCodec = (("channelId" | bytes32) :: ("feeratePerKw" | feeratePerKw)).as[UpdateFee]

  val announcementSignaturesCodec = {
    ("channelId" | bytes32) ::
      ("shortChannelId" | int64) ::
      ("nodeSignature" | bytes64) ::
      ("bitcoinSignature" | bytes64)
  }.as[AnnouncementSignatures]

  val channelAnnouncementWitnessCodec =
    ("features" | featuresCodec) ::
      ("chainHash" | bytes32) ::
      ("shortChannelId" | int64) ::
      ("nodeId1" | publicKey) ::
      ("nodeId2" | publicKey) ::
      ("bitcoinKey1" | publicKey) ::
      ("bitcoinKey2" | publicKey) ::
      ("unknownFields" | bytes)

  val channelAnnouncementCodec = {
    ("nodeSignature1" | bytes64) ::
      ("nodeSignature2" | bytes64) ::
      ("bitcoinSignature1" | bytes64) ::
      ("bitcoinSignature2" | bytes64) ::
      channelAnnouncementWitnessCodec
  }.as[ChannelAnnouncement]

  val nodeAnnouncementWitnessCodec =
    ("features" | featuresCodec) ::
      ("timestamp" | uint32) ::
      ("nodeId" | publicKey) ::
      ("rgbColor" | rgb) ::
      ("alias" | zeropaddedstring(32)) ::
      ("addresses" | listofnodeaddresses) ::
      ("unknownFields" | bytes)

  val nodeAnnouncementCodec = (("signature" | bytes64) :: nodeAnnouncementWitnessCodec).as[NodeAnnouncement]

  val channelUpdateChecksumCodec =
    ("chainHash" | bytes32) ::
      ("shortChannelId" | int64) ::
      (("messageFlags" | byte) >>:~ { messageFlags =>
        ("channelFlags" | byte) ::
          ("cltvExpiryDelta" | cltvExpiryDelta) ::
          ("htlcMinimumMsat" | millisatoshi) ::
          ("feeBaseMsat" | millisatoshi32) ::
          ("feeProportionalMillionths" | uint32) ::
          ("htlcMaximumMsat" | conditional((messageFlags & 1) != 0, millisatoshi))
      })

  val channelUpdateWitnessCodec =
    ("chainHash" | bytes32) ::
      ("shortChannelId" | int64) ::
      ("timestamp" | uint32) ::
      (("messageFlags" | byte) >>:~ { messageFlags =>
        ("channelFlags" | byte) ::
          ("cltvExpiryDelta" | cltvExpiryDelta) ::
          ("htlcMinimumMsat" | millisatoshi) ::
          ("feeBaseMsat" | millisatoshi32) ::
          ("feeProportionalMillionths" | uint32) ::
          ("htlcMaximumMsat" | conditional((messageFlags & 1) != 0, millisatoshi)) ::
          ("unknownFields" | bytes)
      })

  val channelUpdateCodec = (("signature" | bytes64) :: channelUpdateWitnessCodec).as[ChannelUpdate]

  val encodedShortChannelIdsCodec: Codec[EncodedShortChannelIds] =
    discriminated[EncodedShortChannelIds].by(byte)
      .\(0) {
        case a@EncodedShortChannelIds(_, Nil) => a // empty list is always encoded with encoding type 'uncompressed' for compatibility with other implementations
        case a@EncodedShortChannelIds(EncodingType.UNCOMPRESSED, _) => a
      }((provide[EncodingType](EncodingType.UNCOMPRESSED) :: list(int64)).as[EncodedShortChannelIds])
      .\(1) {
        case a@EncodedShortChannelIds(EncodingType.COMPRESSED_ZLIB, _) => a
      }((provide[EncodingType](EncodingType.COMPRESSED_ZLIB) :: zlib(list(int64))).as[EncodedShortChannelIds])


  val queryShortChannelIdsCodec = {
    ("chainHash" | bytes32) ::
      ("shortChannelIds" | variableSizeBytes(uint16, encodedShortChannelIdsCodec)) ::
      ("tlvStream" | QueryShortChannelIdsTlv.codec)
  }.as[QueryShortChannelIds]

  val replyShortChanelIdsEndCodec = {
    ("chainHash" | bytes32) ::
      ("complete" | byte)
  }.as[ReplyShortChannelIdsEnd]

  val queryChannelRangeCodec = {
    ("chainHash" | bytes32) ::
      ("firstBlockNum" | uint32) ::
      ("numberOfBlocks" | uint32) ::
      ("tlvStream" | QueryChannelRangeTlv.codec)
  }.as[QueryChannelRange]

  val replyChannelRangeCodec = {
    ("chainHash" | bytes32) ::
      ("firstBlockNum" | uint32) ::
      ("numberOfBlocks" | uint32) ::
      ("syncComplete" | byte) ::
      ("shortChannelIds" | variableSizeBytes(uint16, encodedShortChannelIdsCodec)) ::
      ("tlvStream" | ReplyChannelRangeTlv.codec)
  }.as[ReplyChannelRange]

  val gossipTimestampFilterCodec = {
    ("chainHash" | bytes32) ::
      ("firstTimestamp" | uint32) ::
      ("timestampRange" | uint32)
  }.as[GossipTimestampFilter]

  val unknownMessageCodec = {
    ("tag" | uint16) ::
      ("message" | varsizebinarydata)
  }.as[UnknownMessage]

  // HOSTED CHANNELS
  val tickerCodec = {
    val tag: Codec[Ticker] = variableSizeBytes(uint16, utf8).narrow(tag => Attempt.fromOption(Ticker.tickerByTag(tag), Err.apply(s"Unknown ticker ${tag}")), _.tag)
    tag withContext "tag"
  }.as[Ticker]

//  val paymentRequestCodec = {
//    variableSizeBytes(uint16, utf8).narrow(invoice => Attempt.Successful(PaymentRequest.read(invoice)), (invoice: PaymentRequest) => PaymentRequest.write(invoice))
//  }.as[PaymentRequest]

  val invokeHostedChannelCodec = {
    (bytes32 withContext "chainHash") ::
      (varsizebinarydata withContext "refundScriptPubKey") ::
      (varsizebinarydata withContext "secret") ::
      (tickerCodec withContext "ticker")
  }.as[InvokeHostedChannel]

  lazy val initHostedChannelCodec = {
    (uint64 withContext "maxHtlcValueInFlightMsat") ::
      (millisatoshi withContext "htlcMinimumMsat") ::
      (uint16 withContext "maxAcceptedHtlcs") ::
      (millisatoshi withContext "channelCapacityMsat") ::
      (millisatoshi withContext "initialClientBalanceMsat") ::
      (millisatoshi withContext "initialRate") ::
      (tickerCodec withContext "ticker") ::
      (listOfN(uint16, uint16) withContext "features")
  }.as[InitHostedChannel]

  lazy val hostedChannelBrandingCodec = {
    (rgb withContext "rgbColor") ::
      (optional(bool8, varsizebinarydata) withContext "pngIcon") ::
      (variableSizeBytes(uint16, utf8) withContext "contactInfo")
  }.as[HostedChannelBranding]

  lazy val lastCrossSignedStateCodec = {
    (bool8 withContext "isHost") ::
      (varsizebinarydata withContext "refundScriptPubKey") ::
      (lengthDelimited(initHostedChannelCodec) withContext "initHostedChannel") ::
      (uint32 withContext "blockDay") ::
      (millisatoshi withContext "localBalanceMsat") ::
      (millisatoshi withContext "remoteBalanceMsat") ::
      (millisatoshi withContext "rate") ::
      (uint32 withContext "localUpdates") ::
      (uint32 withContext "remoteUpdates") ::
      (listOfN(uint16, lengthDelimited(LightningMessageCodecs.updateAddHtlcCodec)) withContext "incomingHtlcs") ::
      (listOfN(uint16, lengthDelimited(LightningMessageCodecs.updateAddHtlcCodec)) withContext "outgoingHtlcs") ::
      (bytes64 withContext "remoteSigOfLocal") ::
      (bytes64 withContext "localSigOfRemote")
  }.as[LastCrossSignedState]

  val stateUpdateCodec = {
    (uint32 withContext "blockDay") ::
      (uint32 withContext "localUpdates") ::
      (uint32 withContext "remoteUpdates") ::
      (millisatoshi withContext "rate") ::
      (bytes64 withContext "localSigOfRemoteLCSS")
  }.as[StateUpdate]

  val stateOverrideCodec = {
    (uint32 withContext "blockDay") ::
      (millisatoshi withContext "localBalanceMsat") ::
      (uint32 withContext "localUpdates") ::
      (uint32 withContext "remoteUpdates") ::
      (millisatoshi withContext "rate") ::
      (bytes64 withContext "localSigOfRemoteLCSS")
  }.as[StateOverride]

  lazy val announcementSignatureCodec = {
    (bytes64 withContext "nodeSignature") ::
      (bool8 withContext "wantsReply")
  }.as[AnnouncementSignature]

  val resizeChannelCodec = {
    (satoshi withContext "newCapacity") ::
      (bytes64 withContext "clientSig")
  }.as[ResizeChannel]

  val marginChannelCodec = {
    (satoshi withContext "newCapacity") ::
    (millisatoshi withContext "newRate") ::
      (bytes64 withContext "clientSig")
  }.as[MarginChannel]

  val askBrandingInfoCodec = (bytes32 withContext "chainHash").as[AskBrandingInfo]

  val queryPublicHostedChannelsCodec = (bytes32 withContext "chainHash").as[QueryPublicHostedChannels]

  val replyPublicHostedChannelsEndCodec = (bytes32 withContext "chainHash").as[ReplyPublicHostedChannelsEnd]

  lazy val queryPreimagesCodec = (listOfN(uint16, bytes32) withContext "hashes").as[QueryPreimages]

  lazy val replyPreimagesCodec = (listOfN(uint16, bytes32) withContext "preimages").as[ReplyPreimages]

  lazy val replyCurrentRateCodec = (millisatoshi withContext "rate").as[ReplyCurrentRate]

//  lazy val proposeInvoiceCodec = {
//    (variableSizeBytes(uint16, utf8) withContext "description") ::
//      (paymentRequestCodec withContext "invoice")
//  }.as[ProposeInvoice]

  final val HC_INVOKE_HOSTED_CHANNEL_TAG = 55535

  final val HC_INIT_HOSTED_CHANNEL_TAG = 55533

  final val HC_LAST_CROSS_SIGNED_STATE_TAG = 55531

  final val HC_STATE_UPDATE_TAG = 55529

  final val HC_STATE_OVERRIDE_TAG = 55527

  final val HC_HOSTED_CHANNEL_BRANDING_TAG = 55525

  final val HC_ANNOUNCEMENT_SIGNATURE_TAG = 55523

  final val HC_RESIZE_CHANNEL_TAG = 55521

  final val HC_QUERY_PUBLIC_HOSTED_CHANNELS_TAG = 55519

  final val HC_REPLY_PUBLIC_HOSTED_CHANNELS_END_TAG = 55517

  final val HC_QUERY_PREIMAGES_TAG = 55515

  final val HC_REPLY_PREIMAGES_TAG = 55513

  final val HC_ASK_BRANDING_INFO = 55511


  final val PHC_ANNOUNCE_GOSSIP_TAG = 54513

  final val PHC_ANNOUNCE_SYNC_TAG = 54511

  final val PHC_UPDATE_GOSSIP_TAG = 54509

  final val PHC_UPDATE_SYNC_TAG = 54507

  final val HC_UPDATE_ADD_HTLC_TAG = 53505

  final val HC_UPDATE_FULFILL_HTLC_TAG = 53503

  final val HC_UPDATE_FAIL_HTLC_TAG = 53501

  final val HC_UPDATE_FAIL_MALFORMED_HTLC_TAG = 53499

  final val HC_ERROR_TAG = 53497


  final val HC_QUERY_RATE_TAG = 52513

  final val HC_REPLY_RATE_TAG = 52511

  final val HC_PROPOSE_INVOICE_TAG = 52515

  final val HC_MARGIN_CHANNEL_TAG = 53495

  // SWAP-IN

  val swapInResponseCodec = {
    ("btcAddress" | text) ::
      ("minChainDeposit" | satoshi)
  }.as[SwapInResponse]

  val swapInPaymentRequestCodec = {
    ("paymentRequest" | text) ::
      ("id" | uint32)
  }.as[SwapInPaymentRequest]

  val swapInPaymentDeniedCodec = {
    ("id" | uint32) ::
      ("reason" | uint32)
  }.as[SwapInPaymentDenied]

  val pendingDepositCodec = {
    ("id" | uint32) ::
      ("lnPaymentId" | optionalText) ::
      ("lnStatus" | uint32) ::
      ("btcAddress" | text) ::
      ("outIndex" | uint32) ::
      ("txid" | text) ::
      ("amountSat" | uint32) ::
      ("depth" | uint32) ::
      ("stamp" | uint32)
  }.as[ChainDeposit]

  val swapInStateCodec = {
    ("pending" | listOfN(uint16, pendingDepositCodec)) ::
      ("ready" | listOfN(uint16, pendingDepositCodec)) ::
      ("processing" | listOfN(uint16, pendingDepositCodec))
  }.as[SwapInState]

  // TRAMPOLINE STATUS

  val trampolineOnCodec = {
    (millisatoshi withContext "minMsat") ::
      (millisatoshi withContext "maxMsat") ::
      (uint32 withContext "feeProportionalMillionths") ::
      (double withContext "exponent") ::
      (double withContext "logExponent") ::
      (cltvExpiryDelta withContext "cltvExpiryDelta")
  }.as[TrampolineOn]

  //

  val lightningMessageCodec: DiscriminatorCodec[LightningMessage, Int] =
    discriminated[LightningMessage].by(uint16)
      .typecase(1, warningCodec)
      .typecase(16, initCodec)
      .typecase(17, failCodec)
      .typecase(18, pingCodec)
      .typecase(19, pongCodec)
      .typecase(32, openChannelCodec)
      .typecase(33, acceptChannelCodec)
      .typecase(34, fundingCreatedCodec)
      .typecase(35, fundingSignedCodec)
      .typecase(36, fundingLockedCodec)
      .typecase(38, shutdownCodec)
      .typecase(39, closingSignedCodec)
      .typecase(128, updateAddHtlcCodec)
      .typecase(130, updateFulfillHtlcCodec)
      .typecase(131, updateFailHtlcCodec)
      .typecase(132, commitSigCodec)
      .typecase(133, revokeAndAckCodec)
      .typecase(134, updateFeeCodec)
      .typecase(135, updateFailMalformedHtlcCodec)
      .typecase(136, channelReestablishCodec)
      .typecase(256, channelAnnouncementCodec)
      .typecase(257, nodeAnnouncementCodec)
      .typecase(258, channelUpdateCodec)
      .typecase(259, announcementSignaturesCodec)
      .typecase(261, queryShortChannelIdsCodec)
      .typecase(262, replyShortChanelIdsEndCodec)
      .typecase(263, queryChannelRangeCodec)
      .typecase(264, replyChannelRangeCodec)
      .typecase(265, gossipTimestampFilterCodec)

  //

  val lightningMessageCodecWithFallback: Codec[LightningMessage] =
    discriminatorWithDefault(lightningMessageCodec, unknownMessageCodec.upcast)

  // EXTENDED MESSAGE UTILS

  def decode(msg: UnknownMessage): LightningMessage = {
    val codec = msg.tag match {
      case HC_HOSTED_CHANNEL_BRANDING_TAG => hostedChannelBrandingCodec
      case HC_LAST_CROSS_SIGNED_STATE_TAG => lastCrossSignedStateCodec
      case HC_INVOKE_HOSTED_CHANNEL_TAG => invokeHostedChannelCodec
      case HC_INIT_HOSTED_CHANNEL_TAG => initHostedChannelCodec
      case HC_STATE_OVERRIDE_TAG => stateOverrideCodec
      case HC_RESIZE_CHANNEL_TAG => resizeChannelCodec
      case HC_MARGIN_CHANNEL_TAG => marginChannelCodec
      case HC_STATE_UPDATE_TAG => stateUpdateCodec

      case HC_QUERY_PUBLIC_HOSTED_CHANNELS_TAG => queryPublicHostedChannelsCodec
      case HC_REPLY_PUBLIC_HOSTED_CHANNELS_END_TAG => replyPublicHostedChannelsEndCodec
      case HC_QUERY_PREIMAGES_TAG => queryPreimagesCodec
      case HC_REPLY_PREIMAGES_TAG => replyPreimagesCodec
      case HC_ASK_BRANDING_INFO => askBrandingInfoCodec

      case PHC_ANNOUNCE_GOSSIP_TAG => channelAnnouncementCodec
      case PHC_ANNOUNCE_SYNC_TAG => channelAnnouncementCodec
      case PHC_UPDATE_GOSSIP_TAG => channelUpdateCodec
      case PHC_UPDATE_SYNC_TAG => channelUpdateCodec

      case HC_UPDATE_FAIL_MALFORMED_HTLC_TAG => updateFailMalformedHtlcCodec
      case HC_UPDATE_FULFILL_HTLC_TAG => updateFulfillHtlcCodec
      case HC_UPDATE_FAIL_HTLC_TAG => updateFailHtlcCodec
      case HC_UPDATE_ADD_HTLC_TAG => updateAddHtlcCodec
      case HC_ERROR_TAG => failCodec

      case HC_QUERY_RATE_TAG => provide(QueryCurrentRate())
      case HC_REPLY_RATE_TAG => replyCurrentRateCodec
      //case HC_PROPOSE_INVOICE_TAG => proposeInvoiceCodec

      case _ => throw new RuntimeException
    }

    codec.decode(msg.data.toBitVector).require.value
  }

  // Extended messages need to be wrapped in UnknownMessage

  def prepare(msg: LightningMessage): LightningMessage = msg match {
    case msg: HostedChannelBranding => UnknownMessage(HC_HOSTED_CHANNEL_BRANDING_TAG, hostedChannelBrandingCodec.encode(msg).require.toByteVector)
    case msg: LastCrossSignedState => UnknownMessage(HC_LAST_CROSS_SIGNED_STATE_TAG, lastCrossSignedStateCodec.encode(msg).require.toByteVector)
    case msg: InvokeHostedChannel => UnknownMessage(HC_INVOKE_HOSTED_CHANNEL_TAG, invokeHostedChannelCodec.encode(msg).require.toByteVector)
    case msg: InitHostedChannel => UnknownMessage(HC_INIT_HOSTED_CHANNEL_TAG, initHostedChannelCodec.encode(msg).require.toByteVector)
    case msg: StateOverride => UnknownMessage(HC_STATE_OVERRIDE_TAG, stateOverrideCodec.encode(msg).require.toByteVector)
    case msg: ResizeChannel => UnknownMessage(HC_RESIZE_CHANNEL_TAG, resizeChannelCodec.encode(msg).require.toByteVector)
    case msg: MarginChannel => UnknownMessage(HC_MARGIN_CHANNEL_TAG, marginChannelCodec.encode(msg).require.toByteVector)
    case msg: StateUpdate => UnknownMessage(HC_STATE_UPDATE_TAG, stateUpdateCodec.encode(msg).require.toByteVector)

    case msg: QueryPublicHostedChannels => UnknownMessage(HC_QUERY_PUBLIC_HOSTED_CHANNELS_TAG, queryPublicHostedChannelsCodec.encode(msg).require.toByteVector)
    case msg: ReplyPublicHostedChannelsEnd => UnknownMessage(HC_REPLY_PUBLIC_HOSTED_CHANNELS_END_TAG, replyPublicHostedChannelsEndCodec.encode(msg).require.toByteVector)
    case msg: QueryPreimages => UnknownMessage(HC_QUERY_PREIMAGES_TAG, queryPreimagesCodec.encode(msg).require.toByteVector)
    case msg: ReplyPreimages => UnknownMessage(HC_REPLY_PREIMAGES_TAG, replyPreimagesCodec.encode(msg).require.toByteVector)
    case msg: AskBrandingInfo => UnknownMessage(HC_ASK_BRANDING_INFO, askBrandingInfoCodec.encode(msg).require.toByteVector)
    case msg: QueryCurrentRate => UnknownMessage(HC_QUERY_RATE_TAG, provide(QueryCurrentRate).encode(QueryCurrentRate).require.toByteVector)
    case msg: ReplyCurrentRate => UnknownMessage(HC_REPLY_RATE_TAG, replyCurrentRateCodec.encode(msg).require.toByteVector)
    //case msg: ProposeInvoice => UnknownMessage(HC_PROPOSE_INVOICE_TAG, proposeInvoiceCodec.encode(msg).require.toByteVector)

    case _ => msg
  }

  // HC uses the following protocol-defined messages, but they still need to be wrapped in UnknownMessage

  def prepareNormal(msg: LightningMessage): LightningMessage = msg match {
    case msg: Fail => UnknownMessage(HC_ERROR_TAG, LightningMessageCodecs.failCodec.encode(msg).require.toByteVector)
    case msg: ChannelUpdate => UnknownMessage(PHC_UPDATE_SYNC_TAG, LightningMessageCodecs.channelUpdateCodec.encode(msg).require.toByteVector)
    case msg: UpdateAddHtlc => UnknownMessage(HC_UPDATE_ADD_HTLC_TAG, LightningMessageCodecs.updateAddHtlcCodec.encode(msg).require.toByteVector)
    case msg: UpdateFailHtlc => UnknownMessage(HC_UPDATE_FAIL_HTLC_TAG, LightningMessageCodecs.updateFailHtlcCodec.encode(msg).require.toByteVector)
    case msg: UpdateFulfillHtlc => UnknownMessage(HC_UPDATE_FULFILL_HTLC_TAG, LightningMessageCodecs.updateFulfillHtlcCodec.encode(msg).require.toByteVector)
    case msg: UpdateFailMalformedHtlc => UnknownMessage(HC_UPDATE_FAIL_MALFORMED_HTLC_TAG, LightningMessageCodecs.updateFailMalformedHtlcCodec.encode(msg).require.toByteVector)
    case _ => msg
  }
}