package fr.acinq.eclair.blockchain.electrum

import akka.actor.{FSM, PoisonPill}
import fr.acinq.bitcoin.BlockHeader
import fr.acinq.eclair.blockchain.electrum.Blockchain.RETARGETING_PERIOD
import fr.acinq.eclair.blockchain.electrum.ElectrumClient.GetHeaders
import fr.acinq.eclair.blockchain.electrum.ElectrumWallet.{DISCONNECTED, RUNNING, SYNCING, WAITING_TIP}
import java.io.InputStream
import scala.util.Try


trait ChainSyncEvent
case class ChainSyncing(initialLocalHeight: Int, localHeight: Int, remoteHeight: Int) extends ChainSyncEvent
case class ChainSyncEnded(oldLocalHeight: Int, blockchain: Blockchain) extends ChainSyncEvent
case object ChainReorganized extends ChainSyncEvent

class ElectrumChainSync(electrum: Electrum, stream: InputStream, strict: Boolean) extends FSM[ElectrumWallet.State, Blockchain] {
  def freshChain: Blockchain = Blockchain(enforceSameBits = strict, checkpoints = bundled, headersMap = Map.empty)
  lazy val bundled: Vector[CheckPoint] = CheckPoint.load(stream)

  def resetAfterReorg = {
    electrum.params.dataDb.db.txWrap {
      electrum.params.dataDb.headerCleanUp
      electrum.params.txDb.cleanUp
    }

    electrum.specs.values.foreach(_.walletRef ! ChainReorganized)
    goto(DISCONNECTED) using freshChain replying PoisonPill
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
      if blockchain.bestchain.nonEmpty && response.height < blockchain.tip.height =>
      goto(DISCONNECTED) replying PoisonPill

    case Event(response: ElectrumClient.HeaderSubscriptionResponse, blockchain) if blockchain.bestchain.isEmpty =>
      electrum.pool ! ElectrumClient.GetHeaders(blockchain.checkpoints.size * RETARGETING_PERIOD, RETARGETING_PERIOD)
      initialLocalHeight = blockchain.checkpoints.size * RETARGETING_PERIOD - 1
      reportedHeight = response.height
      goto(SYNCING)

    case Event(response: ElectrumClient.HeaderSubscriptionResponse, blockchain) if response.header == blockchain.tip.header =>
      context.system.eventStream publish ChainSyncEnded(oldLocalHeight = blockchain.tip.height, blockchain)
      context.system.eventStream publish blockchain
      goto(RUNNING)

    case Event(response: ElectrumClient.HeaderSubscriptionResponse, blockchain) =>
      electrum.pool ! ElectrumClient.GetHeaders(blockchain.tip.height + 1, RETARGETING_PERIOD)
      initialLocalHeight = blockchain.tip.height
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
      context.system.eventStream publish ChainSyncing(initialLocalHeight, blockchain.tip.height, reportedHeight)
      electrum.pool ! ElectrumClient.GetHeaders(blockchain1.tip.height + 1, RETARGETING_PERIOD)
      goto(SYNCING) using blockchain1
    } catch {
      case _: Throwable =>
        goto(DISCONNECTED) replying PoisonPill
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
      context.system.eventStream publish ChainSyncEnded(blockchain.tip.height, blockchain1)
      context.system.eventStream publish blockchain1
      stay using blockchain1
    } catch {
      case _: Throwable =>
        stay replying PoisonPill
    }

    case Event(ElectrumClient.GetHeadersResponse(start, headers, _), blockchain) => try {
      val blockchain1 = Blockchain.addHeaders(blockchain, start, headers)
      electrum.params.dataDb.addHeaders(headers, start)
      context.system.eventStream publish blockchain1
      stay using blockchain1
    } catch {
      case _: Throwable =>
        stay replying PoisonPill
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
