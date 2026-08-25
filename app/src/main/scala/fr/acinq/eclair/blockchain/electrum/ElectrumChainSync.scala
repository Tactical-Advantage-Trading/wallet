package fr.acinq.eclair.blockchain.electrum

import fr.acinq.eclair.blockchain.electrum.Blockchain.RETARGETING_PERIOD
import fr.acinq.eclair.blockchain.electrum.ElectrumClient.GetHeaders
import fr.acinq.eclair.blockchain.electrum.ElectrumWallet.{DISCONNECTED, RUNNING, SYNCING, WAITING_TIP}
import trading.tacticaladvantage.Tools.ThrowableOps
import java.io.InputStream
import scala.util.Try


trait ChainSyncEvent
case class ChainSyncing(initialLocalHeight: Int, localHeight: Int, remoteHeight: Int) extends ChainSyncEvent
case class ChainSyncEnded(oldLocalHeight: Int, blockchain: Blockchain) extends ChainSyncEvent
case object ChainReorganized extends ChainSyncEvent

class ElectrumChainSync(electrum: Electrum, stream: InputStream, enforceSameBitsAfterHeight: Int) extends akka.actor.FSM[ElectrumWallet.State, Blockchain] {
  def freshChain: Blockchain = Blockchain(enforceSameBitsAfterHeight, checkpoints = bundled, headersMap = Map.empty)
  lazy val bundled: Vector[CheckPoint] = CheckPoint.load(stream)

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
    val checkpoints = CheckPoint.withDbHeaders(bundled, electrum.params.dataDb)
    val startHeight = checkpoints.size * RETARGETING_PERIOD

    val blockchain0 = freshChain.copy(checkpoints = checkpoints)
    val headers = electrum.params.dataDb.getHeaders(startHeight, Int.MaxValue)
    val blockchain1 = Try apply Blockchain.addHeadersChunk(blockchain0, startHeight, headers)
    startWith(DISCONNECTED, blockchain1 getOrElse blockchain0)
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
      electrum.pool ! ElectrumClient.GetHeaders(blockchain.checkpoints.size * RETARGETING_PERIOD, RETARGETING_PERIOD)
      initialLocalHeight = blockchain.checkpoints.size * RETARGETING_PERIOD - 1
      reportedHeight = response.height
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
