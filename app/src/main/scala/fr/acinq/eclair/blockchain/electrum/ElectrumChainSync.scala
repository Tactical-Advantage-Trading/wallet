package fr.acinq.eclair.blockchain.electrum

import fr.acinq.bitcoin.BlockHeader
import fr.acinq.eclair.blockchain.electrum.ElectrumClient.GetHeaders
import fr.acinq.eclair.blockchain.electrum.ElectrumWallet.{DISCONNECTED, RUNNING, SYNCING, WAITING_TIP}
import fr.acinq.eclair.blockchain.electrum.Blockchain.{ECX_RETARGETING_PERIOD, RETARGETING_PERIOD}
import trading.tacticaladvantage.Tools.ThrowableOps

import java.io.InputStream
import scala.util.Try


trait ChainSyncEvent
case class ChainSyncing(initialLocalHeight: Int, localHeight: Int, remoteHeight: Int) extends ChainSyncEvent
case class ChainSyncEnded(oldLocalHeight: Int, blockchain: Blockchain) extends ChainSyncEvent
case object ChainReorganized extends ChainSyncEvent

class ElectrumChainSync(electrum: Electrum, stream: InputStream, retargetPeriod: Int) extends akka.actor.FSM[ElectrumWallet.State, Blockchain] {
  private def needsCheckpointHeader(bc: Blockchain): Boolean = bc.retargetPeriod == ECX_RETARGETING_PERIOD && !bc.headersMap.contains(bc.checkpoints.last.hash)
  private def freshChain: Blockchain = Blockchain(retargetPeriod, checkpoints = bundled, headersMap = Map.empty)
  lazy val bundled: Vector[CheckPoint] = CheckPoint.load(stream)

  private def withCheckpointHeader(blockchain: Blockchain, header: BlockHeader): Blockchain = {
    require(header.hash == blockchain.checkpoints.last.hash, "Wrong checkpoint header")
    require(BlockHeader.checkProofOfWork(header), "Invalid checkpoint PoW")

    val index = Blockchain.BlockIndex(header, blockchain.checkpoints.size * RETARGETING_PERIOD - 1)
    blockchain.copy(headersMap = Map(index.hash -> index) ++ blockchain.headersMap)
  }

  def resetAfterReorg = {
    electrum.params.dataDb.db.txWrap {
      electrum.params.dataDb.headerCleanUp
      electrum.params.txDb.cleanUp
    }

    log.info("[KILLPEER] reorg")
    context.system.eventStream publish ChainReorganized
    electrum.specs.values.foreach(_.walletRef ! ChainReorganized)
    goto(DISCONNECTED) using freshChain replying akka.actor.PoisonPill
  }

  var reportedHeight = 0
  var initialLocalHeight = 0

  {
    require(retargetPeriod == RETARGETING_PERIOD || retargetPeriod == ECX_RETARGETING_PERIOD)
    val checkpoints = CheckPoint.withDbHeaders(bundled, electrum.params.dataDb)
    val startHeight = checkpoints.size * RETARGETING_PERIOD
    val base = freshChain.copy(checkpoints = checkpoints)

    val anchored = if (retargetPeriod == ECX_RETARGETING_PERIOD) {
      electrum.params.dataDb.getHeader(startHeight - 1) map { header =>
        withCheckpointHeader(base, header)
      } getOrElse base
    } else base

    val needsHeader = needsCheckpointHeader(anchored)
    val restored = if (needsHeader) anchored else Try {
      val headers = electrum.params.dataDb.getHeaders(startHeight, Int.MaxValue)
      Blockchain.addHeadersChunk(anchored, startHeight, headers)
    } getOrElse anchored

    startWith(DISCONNECTED, restored)
    electrum.pool ! ElectrumClient.AddStatusListener(self)
  }

  when(DISCONNECTED) {
    case Event(_: ElectrumClient.ElectrumReady, _) =>
      electrum.pool ! ElectrumClient.HeaderSubscription(self)
      goto(WAITING_TIP)
  }

  when(WAITING_TIP) {
    case Event(response: ElectrumClient.HeaderSubscriptionResponse, blockchain)
      if blockchain.isReorg(response.height, response.header, electrum.params.dataDb) =>
      resetAfterReorg

    case Event(response: ElectrumClient.HeaderSubscriptionResponse, blockchain)
      if blockchain.bestchain.nonEmpty && response.height < blockchain.height =>
      log.info(s"[KILLPEER] stale remote=${response.height}, local=${blockchain.height}")
      goto(DISCONNECTED) replying akka.actor.PoisonPill

    case Event(response: ElectrumClient.HeaderSubscriptionResponse, blockchain) if blockchain.bestchain.isEmpty =>
      val startHeight = blockchain.checkpoints.size * RETARGETING_PERIOD
      initialLocalHeight = startHeight - 1
      reportedHeight = response.height

      val needsHeader = needsCheckpointHeader(blockchain)
      if (needsHeader) electrum.pool ! ElectrumClient.GetHeader(startHeight - 1)
      else electrum.pool ! ElectrumClient.GetHeaders(startHeight, RETARGETING_PERIOD)
      goto(SYNCING)

    case Event(response: ElectrumClient.HeaderSubscriptionResponse, blockchain) if response.header == blockchain.tip.header =>
      context.system.eventStream publish ChainSyncEnded(oldLocalHeight = blockchain.height, blockchain)
      context.system.eventStream publish blockchain
      goto(RUNNING)

    case Event(response: ElectrumClient.HeaderSubscriptionResponse, blockchain) =>
      electrum.pool ! ElectrumClient.GetHeaders(blockchain.height + 1, RETARGETING_PERIOD)
      initialLocalHeight = blockchain.height
      reportedHeight = response.height
      goto(SYNCING)
  }

  when(SYNCING) {
    case Event(ElectrumClient.GetHeaderResponse(height, header), blockchain)
      if needsCheckpointHeader(blockchain) && height == blockchain.checkpoints.size * RETARGETING_PERIOD - 1 =>

      try {
        val blockchain1 = withCheckpointHeader(blockchain, header)
        electrum.params.dataDb.addHeaders(headers = Seq(header), height)
        electrum.pool ! ElectrumClient.GetHeaders(height + 1, RETARGETING_PERIOD)
        stay using blockchain1
      } catch {
        case err: Throwable =>
          log.info(s"[KILLPEER] e0 ${err.stackTraceString}")
          goto(DISCONNECTED) replying akka.actor.PoisonPill
      }

    case Event(ElectrumClient.GetHeadersResponse(startHeight, header +: _, _), blockchain)
      if blockchain.isReorg(startHeight, header, electrum.params.dataDb) =>
      resetAfterReorg

    case Event(response: ElectrumClient.GetHeadersResponse, blockchain) if response.headers.isEmpty =>
      context.system.eventStream publish ChainSyncEnded(initialLocalHeight, blockchain)
      context.system.eventStream publish blockchain
      goto(RUNNING)

    case Event(ElectrumClient.GetHeadersResponse(start, headers, _), blockchain) => try {
      val (blockchain1, chunks) = Blockchain optimize Blockchain.addHeaders(blockchain, start, headers)
      if (chunks.nonEmpty) electrum.params.dataDb.addHeaders(headers = chunks.map(_.header), chunks.head.height)
      context.system.eventStream publish ChainSyncing(initialLocalHeight, blockchain.height, reportedHeight)
      electrum.pool ! ElectrumClient.GetHeaders(blockchain1.height + 1, RETARGETING_PERIOD)
      goto(SYNCING) using blockchain1
    } catch {
      case err: Throwable =>
        log.info(s"[KILLPEER] e1 ${err.stackTraceString}")
        goto(DISCONNECTED) replying akka.actor.PoisonPill
    }

    case Event(ElectrumClient.HeaderSubscriptionResponse(height, header), _) =>
      log.debug(s"Ignoring header $header at $height while syncing")
      stay
  }

  when(RUNNING) {
    case Event(ElectrumClient.HeaderSubscriptionResponse(height, header), blockchain)
      if blockchain.isReorg(height, header, electrum.params.dataDb) =>
      resetAfterReorg

    case Event(ElectrumClient.HeaderSubscriptionResponse(height, header), blockchain) if blockchain.tip.header != header => try {
      val difficultyOk = Blockchain.getDifficulty(blockchain, height, electrum.params.dataDb).forall(header.bits.==)
      if (!difficultyOk) throw new RuntimeException(f"Wrong difficulty, height=$height, header=$header")
      val (blockchain1, chunks) = Blockchain optimize Blockchain.addHeader(blockchain, height, header)
      if (chunks.nonEmpty) electrum.params.dataDb.addHeaders(chunks.map(_.header), chunks.head.height)
      context.system.eventStream publish ChainSyncEnded(blockchain.height, blockchain1)
      context.system.eventStream publish blockchain1
      stay using blockchain1
    } catch {
      case err: Throwable =>
        log.info(s"[KILLPEER] e2 ${err.stackTraceString}")
        stay replying akka.actor.PoisonPill
    }

    case Event(ElectrumClient.GetHeadersResponse(start, headers, _), blockchain) => try {
      val blockchain1 = Blockchain.addHeaders(blockchain, start, headers)
      electrum.params.dataDb.addHeaders(headers, start)
      context.system.eventStream publish blockchain1
      stay using blockchain1
    } catch {
      case err: Throwable =>
        log.info(s"[KILLPEER] e3 ${err.stackTraceString}")
        stay replying akka.actor.PoisonPill
    }

    case Event(ElectrumWallet.ChainFor(target), blockchain) =>
      target ! blockchain
      stay
  }

  whenUnhandled {
    case Event(getHeaders: GetHeaders, _) =>
      electrum.pool ! getHeaders
      stay

    case Event(ElectrumClient.ElectrumDisconnected, _) =>
      goto(DISCONNECTED)
  }

  initialize
}
