package fr.acinq.eclair.channel

import fr.acinq.bitcoin.Crypto.{PrivateKey, PublicKey}
import fr.acinq.bitcoin.DeterministicWallet._
import fr.acinq.bitcoin._
import fr.acinq.eclair._
import fr.acinq.eclair.blockchain.TxConfirmedAt
import fr.acinq.eclair.blockchain.fee.FeeratePerKw
import fr.acinq.eclair.crypto.Generators
import fr.acinq.eclair.crypto.Sphinx.PacketAndSecrets
import fr.acinq.eclair.payment.IncomingPaymentPacket
import fr.acinq.eclair.transactions.Transactions._
import fr.acinq.eclair.transactions.{CommitmentSpec, Transactions}
import fr.acinq.eclair.wire.PaymentOnion.FinalPayload
import fr.acinq.eclair.wire._
import immortan.crypto.Tools
import immortan.{LNParams, RemoteNodeInfo}
import scodec.bits.ByteVector


sealed trait LocalReject {
  val localAdd: UpdateAddHtlc
}

case class ChannelOffline(localAdd: UpdateAddHtlc) extends LocalReject
case class ChannelNotAbleToSend(localAdd: UpdateAddHtlc) extends LocalReject
case class InPrincipleNotSendable(localAdd: UpdateAddHtlc) extends LocalReject


case class INPUT_INIT_FUNDEE(remoteInfo: RemoteNodeInfo, localParams: LocalParams, remoteInit: Init, channelFeatures: ChannelFeatures, theirOpen: OpenChannel)
case class INPUT_INIT_FUNDER(remoteInfo: RemoteNodeInfo, temporaryChannelId: ByteVector32, fundingAmount: Satoshi, pushAmount: MilliSatoshi, fundingFeeratePerKw: FeeratePerKw,
                             initialFeeratePerKw: FeeratePerKw, localParams: LocalParams, remoteInit: Init, channelFlags: Byte, channelFeatures: ChannelFeatures)


sealed trait BitcoinEvent
case class BITCOIN_PARENT_TX_CONFIRMED(childTx: Transaction) extends BitcoinEvent
case class BITCOIN_TX_CONFIRMED(tx: Transaction) extends BitcoinEvent
case object BITCOIN_FUNDING_DEPTHOK extends BitcoinEvent
case object BITCOIN_FUNDING_SPENT extends BitcoinEvent
case object BITCOIN_OUTPUT_SPENT extends BitcoinEvent


sealed trait IncomingResolution

sealed trait ReasonableResolution extends IncomingResolution {
  val fullTag: FullPaymentTag // Payment type and grouping data (paymentHash x paymentSecret x type)
  val secret: PrivateKey // Node secret whose pubKey is seen by peer (might be peer-specific or invoice-specific)
  val add: UpdateAddHtlc
}

case class ReasonableTrampoline(packet: IncomingPaymentPacket.NodeRelayPacket, secret: PrivateKey) extends ReasonableResolution {
  val fullTag: FullPaymentTag = FullPaymentTag(packet.add.paymentHash, packet.outerPayload.paymentSecret, PaymentTagTlv.TRAMPLOINE_ROUTED)
  val add: UpdateAddHtlc = packet.add
}

case class ReasonableLocal(packet: IncomingPaymentPacket.FinalPacket, secret: PrivateKey) extends ReasonableResolution {
  val fullTag: FullPaymentTag = FullPaymentTag(packet.add.paymentHash, packet.payload.paymentSecret, PaymentTagTlv.FINAL_INCOMING)
  val add: UpdateAddHtlc = packet.add
}

sealed trait Command

sealed trait FinalResolution extends IncomingResolution with Command {
  val theirAdd: UpdateAddHtlc
}

case class CMD_FULFILL_HTLC(preimage: ByteVector32, theirAdd: UpdateAddHtlc) extends FinalResolution

case class CMD_FAIL_MALFORMED_HTLC(onionHash: ByteVector32, failureCode: Int, theirAdd: UpdateAddHtlc) extends FinalResolution

case class CMD_FAIL_HTLC(reason: Either[ByteVector, FailureMessage], nodeSecret: PrivateKey, theirAdd: UpdateAddHtlc) extends FinalResolution

case class CMD_ADD_HTLC(fullTag: FullPaymentTag, firstAmount: MilliSatoshi, cltvExpiry: CltvExpiry, packetAndSecrets: PacketAndSecrets, payload: FinalPayload) extends Command {
  val incompleteAdd: UpdateAddHtlc = UpdateAddHtlc(channelId = ByteVector32.Zeroes, id = 0L, firstAmount, fullTag.paymentHash, cltvExpiry, packetAndSecrets.packet, encryptedTag)

  lazy val encryptedTag: PaymentTagTlv.EncryptedSecretStream = {
    val shortTag = ShortPaymentTag(fullTag.paymentSecret, fullTag.tag)
    val plainBytes = PaymentTagTlv.shortPaymentTagCodec.encode(shortTag).require.toByteVector
    val cipherbytes = Tools.chaChaEncrypt(LNParams.secret.keys.ourNodePrivateKey.value, randomBytes(12), plainBytes)
    TlvStream(EncryptedPaymentSecret(cipherbytes) :: Nil)
  }
}

object CMD_CLOSE {
  final val AWAITING_REMOTE_FORCE_CLOSE = "awaiting-remote-force-close"
  final val INVALID_CLOSING_PUBKEY = "invalid-closing-pubkey"
  final val ALREADY_IN_PROGRESS = "already-in-progress"
  final val CHANNEL_BUSY = "channel-busy"
}

case class CMD_CLOSE(scriptPubKey: Option[ByteVector], force: Boolean) extends Command
case class CMD_HOSTED_STATE_OVERRIDE(so: StateOverride) extends Command
case class CMD_HOSTED_QUERY_RATE() extends Command
case class HC_CMD_RESIZE(delta: Satoshi) extends Command
case class HC_CMD_MARGIN(newCapacity: Satoshi, newRate: MilliSatoshi) extends Command
case object CMD_SOCKET_OFFLINE extends Command
case object CMD_SOCKET_ONLINE extends Command
case object CMD_CHECK_FEERATE extends Command
case object CMD_SIGN extends Command


trait ChannelData {
  def ourBalance: MilliSatoshi = 0L.msat
}

trait PersistentChannelData extends ChannelData {
  def channelId: ByteVector32
}

sealed trait HasNormalCommitments extends PersistentChannelData {
  override def ourBalance: MilliSatoshi = commitments.latestReducedRemoteSpec.toRemote
  override def channelId: ByteVector32 = commitments.channelId
  def withNewCommits(cs: NormalCommits): HasNormalCommitments
  def commitments: NormalCommits
}

case class ClosingTxProposed(unsignedTx: Transaction, localClosingSigned: ClosingSigned)

sealed trait ForceCloseCommitPublished {
  def isIrrevocablySpent(tx: Transaction): Boolean = irrevocablySpent.values.exists(_.tx.txid == tx.txid)
  lazy val isCommitConfirmed: Boolean = isIrrevocablySpent(commitTx)
  val irrevocablySpent: Map[OutPoint, TxConfirmedAt]
  val delayedRefundsLeft: Seq[Transaction]
  val commitTx: Transaction
}

case class LocalCommitPublished(commitTx: Transaction, claimMainDelayedOutputTx: Option[Transaction],
                                htlcSuccessTxs: List[Transaction], htlcTimeoutTxs: List[Transaction], claimHtlcDelayedTxs: List[Transaction],
                                irrevocablySpent: Map[OutPoint, TxConfirmedAt] = Map.empty) extends ForceCloseCommitPublished {

  lazy val delayedRefundsLeft: Seq[Transaction] = (claimMainDelayedOutputTx.toList ++ claimHtlcDelayedTxs).filterNot(isIrrevocablySpent)
}

case class RemoteCommitPublished(commitTx: Transaction, claimMainOutputTx: Option[Transaction],
                                 claimHtlcSuccessTxs: List[Transaction], claimHtlcTimeoutTxs: List[Transaction],
                                 irrevocablySpent: Map[OutPoint, TxConfirmedAt] = Map.empty) extends ForceCloseCommitPublished {

  lazy val delayedRefundsLeft: Seq[Transaction] = claimHtlcTimeoutTxs.filterNot(isIrrevocablySpent)
}

case class RevokedCommitPublished(commitTx: Transaction, claimMainOutputTx: Option[Transaction], mainPenaltyTx: Option[Transaction], htlcPenaltyTxs: List[Transaction],
                                  claimHtlcDelayedPenaltyTxs: List[Transaction], irrevocablySpent: Map[OutPoint, TxConfirmedAt] = Map.empty) extends ForceCloseCommitPublished {

  lazy val delayedRefundsLeft: Seq[Transaction] = claimHtlcDelayedPenaltyTxs.filterNot(isIrrevocablySpent)

  lazy val penaltyTxs: Seq[Transaction] = claimMainOutputTx.toList ++ mainPenaltyTx.toList ++ htlcPenaltyTxs ++ claimHtlcDelayedPenaltyTxs
}

final case class DATA_WAIT_FOR_OPEN_CHANNEL(initFundee: INPUT_INIT_FUNDEE) extends ChannelData

final case class DATA_WAIT_FOR_ACCEPT_CHANNEL(initFunder: INPUT_INIT_FUNDER, lastSent: OpenChannel) extends ChannelData

final case class DATA_WAIT_FOR_FUNDING_INTERNAL(initFunder: INPUT_INIT_FUNDER, remoteParams: RemoteParams, remoteFirstPerCommitmentPoint: PublicKey, lastSent: OpenChannel) extends ChannelData

final case class DATA_WAIT_FOR_FUNDING_CREATED(initFundee: INPUT_INIT_FUNDEE, remoteParams: RemoteParams, lastSent: AcceptChannel) extends ChannelData

final case class DATA_WAIT_FOR_FUNDING_SIGNED(remoteInfo: RemoteNodeInfo, channelId: ByteVector32, localParams: LocalParams, remoteParams: RemoteParams, fundingTx: Transaction,
                                              fundingTxFee: Satoshi, localSpec: CommitmentSpec, localCommitTx: CommitTx, remoteCommit: RemoteCommit, channelFlags: Byte,
                                              channelFeatures: ChannelFeatures, lastSent: FundingCreated) extends ChannelData

final case class DATA_WAIT_FOR_FUNDING_CONFIRMED(commitments: NormalCommits, fundingTx: Option[Transaction], waitingSince: Long, lastSent: Either[FundingCreated, FundingSigned],
                                                 deferred: Option[FundingLocked] = None) extends ChannelData with HasNormalCommitments {

  // Remote peer may send a tx which is unrelated to our agreed upon channel funding, that is, we won't be able to spend our commit tx, check this right away!
  def checkSpend(tx: Transaction): Unit = Transaction.correctlySpends(commitments.localCommit.publishableTxs.commitTx.tx, Seq(tx), ScriptFlags.STANDARD_SCRIPT_VERIFY_FLAGS)
  override def withNewCommits(cs: NormalCommits): HasNormalCommitments = copy(commitments = cs)
}

final case class DATA_WAIT_FOR_FUNDING_LOCKED(commitments: NormalCommits, shortChannelId: Long, lastSent: FundingLocked) extends ChannelData with HasNormalCommitments {
  override def withNewCommits(cs: NormalCommits): HasNormalCommitments = copy(commitments = cs)
}

final case class DATA_NORMAL(commitments: NormalCommits, shortChannelId: Long, feeUpdateRequired: Boolean = false, extParams: List[ByteVector] = Nil,
                             localShutdown: Option[Shutdown] = None, remoteShutdown: Option[Shutdown] = None) extends ChannelData with HasNormalCommitments {

  override def withNewCommits(cs: NormalCommits): HasNormalCommitments = copy(commitments = cs)
}

object DATA_NEGOTIATING {
  type ClosingProposed = List[ClosingTxProposed]
}

final case class DATA_NEGOTIATING(commitments: NormalCommits, localShutdown: Shutdown,
                                  remoteShutdown: Shutdown, closingTxProposed: List[DATA_NEGOTIATING.ClosingProposed] = List(Nil),
                                  bestUnpublishedClosingTxOpt: Option[Transaction] = None) extends ChannelData with HasNormalCommitments {

  def toClosed: DATA_CLOSING = DATA_CLOSING(commitments, System.currentTimeMillis, closingTxProposed.flatten.map(_.unsignedTx), bestUnpublishedClosingTxOpt.toList)
  override def withNewCommits(cs: NormalCommits): HasNormalCommitments = copy(commitments = cs)
}

final case class DATA_CLOSING(commitments: NormalCommits, waitingSince: Long, mutualCloseProposed: List[Transaction] = Nil, mutualClosePublished: List[Transaction] = Nil,
                              localCommitPublished: Option[LocalCommitPublished] = None, remoteCommitPublished: Option[RemoteCommitPublished] = None, nextRemoteCommitPublished: Option[RemoteCommitPublished] = None,
                              futureRemoteCommitPublished: Option[RemoteCommitPublished] = None, revokedCommitPublished: List[RevokedCommitPublished] = Nil) extends ChannelData with HasNormalCommitments {

  lazy val balanceRefunds: Seq[Transaction] =
    // Txs which are not related to HTLC UTXOs but only involved in getting our balance back
    remoteCommitPublished.toList.flatMap(rcp => rcp.commitTx +: rcp.claimMainOutputTx.toList) ++
      nextRemoteCommitPublished.toList.flatMap(rcp => rcp.commitTx +: rcp.claimMainOutputTx.toList) ++
      futureRemoteCommitPublished.toList.flatMap(rcp => rcp.commitTx +: rcp.claimMainOutputTx.toList) ++
      localCommitPublished.toList.flatMap(lcp => lcp.commitTx +: lcp.claimMainDelayedOutputTx.toList) ++
      mutualCloseProposed ++ mutualClosePublished

  lazy val paymentLeftoverRefunds: Seq[Transaction] =
    // Txs which are involved in getting our success/timeout HTLC UTXOs back
    remoteCommitPublished.toList.flatMap(rcp => rcp.claimHtlcSuccessTxs ++ rcp.claimHtlcTimeoutTxs) ++
      nextRemoteCommitPublished.toList.flatMap(rcp => rcp.claimHtlcSuccessTxs ++ rcp.claimHtlcTimeoutTxs) ++
      futureRemoteCommitPublished.toList.flatMap(rcp => rcp.claimHtlcSuccessTxs ++ rcp.claimHtlcTimeoutTxs) ++
      localCommitPublished.toList.flatMap(lcp => lcp.claimHtlcDelayedTxs ++ lcp.htlcSuccessTxs ++ lcp.htlcTimeoutTxs)

  lazy val forceCloseCommitPublished: Option[ForceCloseCommitPublished] = {
    // We must select a single candidate here because its delayed refunds will be displayed to user, so we can't show a total sum of all possible refunds
    val candidates = localCommitPublished ++ remoteCommitPublished ++ nextRemoteCommitPublished ++ futureRemoteCommitPublished ++ revokedCommitPublished
    candidates.find(_.isCommitConfirmed).orElse(candidates.headOption)
  }

  override def withNewCommits(cs: NormalCommits): HasNormalCommitments = copy(commitments = cs)
}

final case class DATA_WAIT_FOR_REMOTE_PUBLISH_FUTURE_COMMITMENT(commitments: NormalCommits, remoteChannelReestablish: ChannelReestablish) extends ChannelData with HasNormalCommitments {
  override def withNewCommits(cs: NormalCommits): HasNormalCommitments = copy(commitments = cs)
}

object ChannelKeys {
  def fromPath(master: ExtendedPrivateKey, path: KeyPath): ChannelKeys = {
    val fundingKey = derivePrivateKey(chain = path.path :+ hardened(0L), parent = master)
    val revocationKey = derivePrivateKey(chain = path.path :+ hardened(1L), parent = master)
    val paymentKey = derivePrivateKey(chain = path.path :+ hardened(2L), parent = master)
    val delayedKey = derivePrivateKey(chain = path.path :+ hardened(3L), parent = master)
    val htlcKey = derivePrivateKey(chain = path.path :+ hardened(4L), parent = master)
    val shaBase = derivePrivateKey(chain = path.path :+ hardened(5L), parent = master)

    val shaSeed = Crypto.sha256(shaBase.privateKey.value :+ 1.toByte)
    ChannelKeys(path, shaSeed, fundingKey, revocationKey, paymentKey, delayedKey, htlcKey)
  }

  def newKeyPath(isFunder: Boolean): KeyPath = {
    def nextHop: Long = secureRandom.nextInt & 0xFFFFFFFFL
    val lastHop = if (isFunder) hardened(1) else hardened(0)
    val path = Seq(nextHop, nextHop, nextHop, nextHop, nextHop, nextHop, nextHop, nextHop, lastHop)
    KeyPath(path)
  }
}

case class ChannelKeys(path: KeyPath, shaSeed: ByteVector32, fundingKey: ExtendedPrivateKey, revocationKey: ExtendedPrivateKey, paymentKey: ExtendedPrivateKey, delayedPaymentKey: ExtendedPrivateKey, htlcKey: ExtendedPrivateKey) {
  def sign(tx: TransactionWithInputInfo, key: PrivateKey, remoteSecret: PrivateKey, txOwner: TxOwner, format: CommitmentFormat): ByteVector64 = Transactions.sign(tx, Generators.revocationPrivKey(key, remoteSecret), txOwner, format)
  def sign(tx: TransactionWithInputInfo, key: PrivateKey, remotePoint: PublicKey, txOwner: TxOwner, format: CommitmentFormat): ByteVector64 = Transactions.sign(tx, Generators.derivePrivKey(key, remotePoint), txOwner, format)
  def commitmentSecret(index: Long): PrivateKey = Generators.perCommitSecret(shaSeed, index)
  def commitmentPoint(index: Long): PublicKey = Generators.perCommitPoint(shaSeed, index)
}

final case class LocalParams(keys: ChannelKeys, dustLimit: Satoshi, maxHtlcValueInFlightMsat: UInt64, channelReserve: Satoshi,
                             htlcMinimum: MilliSatoshi, toSelfDelay: CltvExpiryDelta, maxAcceptedHtlcs: Int, isFunder: Boolean,
                             defaultFinalScriptPubKey: ByteVector, walletStaticPaymentBasepoint: PublicKey)

final case class RemoteParams(dustLimit: Satoshi, maxHtlcValueInFlightMsat: UInt64, channelReserve: Satoshi, htlcMinimum: MilliSatoshi,
                              toSelfDelay: CltvExpiryDelta, maxAcceptedHtlcs: Int, fundingPubKey: PublicKey, revocationBasepoint: PublicKey,
                              paymentBasepoint: PublicKey, delayedPaymentBasepoint: PublicKey, htlcBasepoint: PublicKey, shutdownScript: Option[ByteVector] = None)

// Channel exceptions

case class FeerateTooSmall(channelId: ByteVector32, remoteFeeratePerKw: FeeratePerKw) extends RuntimeException {
  override def toString: String = s"FeerateTooSmall, remoteFeeratePerKw=$remoteFeeratePerKw"
}

case class DustLimitTooSmall(channelId: ByteVector32, dustLimit: Satoshi, min: Satoshi) extends RuntimeException {
  override def toString: String = s"DustLimitTooSmall, dustLimit=$dustLimit, min=$min"
}

case class DustLimitTooLarge(channelId: ByteVector32, dustLimit: Satoshi, max: Satoshi) extends RuntimeException {
  override def toString: String = s"DustLimitTooLarge, dustLimit=$dustLimit, max=$max"
}

case class InvalidMaxAcceptedHtlcs(channelId: ByteVector32, maxAcceptedHtlcs: Int, max: Int) extends RuntimeException {
  override def toString: String = s"InvalidMaxAcceptedHtlcs, maxAcceptedHtlcs=$maxAcceptedHtlcs, max=$max"
}

case class InvalidMinAcceptedHtlcs(channelId: ByteVector32, minAcceptedHtlcs: Int, min: Int) extends RuntimeException {
  override def toString: String = s"InvalidMinAcceptedHtlcs, minAcceptedHtlcs=$minAcceptedHtlcs, min=$min"
}

case class InvalidChainHash(channelId: ByteVector32, local: ByteVector32, remote: ByteVector32) extends RuntimeException {
  override def toString: String = s"InvalidChainHash, local=$local, remote=$remote"
}

case class InvalidPushAmount(channelId: ByteVector32, pushAmount: MilliSatoshi, max: MilliSatoshi) extends RuntimeException {
  override def toString: String = s"InvalidPushAmount, pushAmount=$pushAmount, max=$max"
}

case class ToSelfDelayTooHigh(channelId: ByteVector32, toSelfDelay: CltvExpiryDelta, max: CltvExpiryDelta) extends RuntimeException {
  override def toString: String = s"ToSelfDelayTooHigh, toSelfDelay=$toSelfDelay, max=$max"
}

case class InvalidFundingAmount(channelId: ByteVector32, fundingAmount: Satoshi, min: Satoshi, max: Satoshi) extends RuntimeException {
  override def toString: String = s"InvalidFundingAmount, fundingAmount=$fundingAmount, min=$min, max=$max"
}

case class DustLimitAboveOurChannelReserve(channelId: ByteVector32, dustLimit: Satoshi, channelReserve: Satoshi) extends RuntimeException {
  override def toString: String = s"DustLimitAboveOurChannelReserve, dustLimit=$dustLimit, channelReserve=$channelReserve"
}

case class ChannelReserveBelowOurDustLimit(channelId: ByteVector32, channelReserve: Satoshi, dustLimit: Satoshi) extends RuntimeException {
  override def toString: String = s"ChannelReserveBelowOurDustLimit, channelReserve=$channelReserve, dustLimit=$dustLimit"
}

case class ChannelReserveNotMet(channelId: ByteVector32, toLocal: MilliSatoshi, toRemote: MilliSatoshi, reserve: Satoshi) extends RuntimeException {
  override def toString: String = s"ChannelReserveNotMet, toLocal=$toLocal, toRemote=$toRemote, reserve=$reserve"
}

case class FeerateTooDifferent(channelId: ByteVector32, localFeeratePerKw: FeeratePerKw, remoteFeeratePerKw: FeeratePerKw) extends RuntimeException {
  override def toString: String = s"FeerateTooDifferent, localFeeratePerKw=$localFeeratePerKw, remoteFeeratePerKw=$remoteFeeratePerKw"
}

case class ChannelReserveTooHigh(channelId: ByteVector32, reserveToFundingRatio: Double, maxReserveToFundingRatio: Double) extends RuntimeException {
  override def toString: String = s"DustLimitTooSmall, reserveToFundingRatio=$reserveToFundingRatio, maxReserveToFundingRatio=$maxReserveToFundingRatio"
}

case class MissingChannelType(channelId: ByteVector32) extends RuntimeException { override def toString: String = "MissingChannelType" }

case class InvalidChannelType(channelId: ByteVector32, channelType: ChannelType) extends RuntimeException { override def toString: String = s"InvalidChannelType, channelType=$channelType" }

case class ExpiredHtlcInNormalChannel(channelId: ByteVector32, sentExpiredRouted: Boolean, expiredReceivedRevealed: Boolean) extends RuntimeException {
  override def toString: String = s"ChannelTransitionFail, sentExpiredRouted=$sentExpiredRouted, expiredReceivedRevealed=$expiredReceivedRevealed"
}

case class ChannelTransitionFail(channelId: ByteVector32, message: LightningMessage) extends RuntimeException {
  override def toString: String = s"ChannelTransitionFail, related message=$message"
}

case class RemoteErrorException(details: String) extends RuntimeException {
  override def toString: String = s"RemoteErrorException, details=$details"
}

case class CMDException(reason: String, cmd: Command) extends RuntimeException
