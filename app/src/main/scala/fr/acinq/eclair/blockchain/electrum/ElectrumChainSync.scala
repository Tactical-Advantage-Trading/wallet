package fr.acinq.eclair.blockchain.electrum

import akka.actor.{FSM, PoisonPill}
import fr.acinq.eclair.blockchain.electrum.Blockchain.RETARGETING_PERIOD
import fr.acinq.eclair.blockchain.electrum.ElectrumClient.GetHeaders
import fr.acinq.eclair.blockchain.electrum.ElectrumWallet.{DISCONNECTED, RUNNING, SYNCING, WAITING_FOR_TIP}

import java.io.InputStream
import scala.util.{Failure, Success, Try}


object ElectrumChainSync {
  case class ChainSyncing(initialLocalTip: Int, localTip: Int, remoteTip: Int)
  case class ChainSyncEnded(localTip: Int)
  case object ChainReorganized
}

class ElectrumChainSync(electrum: Electrum, stream: InputStream, strict: Boolean) extends FSM[ElectrumWallet.State, Blockchain] {
  def freshChain: Blockchain = Blockchain(enforceSameBits = strict, checkpoints = bundled, headersMap = Map.empty)
  lazy val bundled: Vector[CheckPoint] = CheckPoint.load(stream)

  def loadChain: Blockchain = {
    val checkpoints = CheckPoint.withDbHeaders(bundled, electrum.params.headerDb)
    val startHeight = checkpoints.size * RETARGETING_PERIOD

    val blockchain = freshChain.copy(checkpoints = checkpoints)
    val headers = electrum.params.headerDb.getHeaders(startHeight, Int.MaxValue)
    Try apply Blockchain.addHeadersChunk(blockchain, startHeight, headers) getOrElse blockchain
  }

  def resetAfterReorg = {
    electrum.params.headerDb.db.txWrap {
      electrum.params.headerDb.cleanUp
      electrum.params.txDb.cleanUp
    }

    electrum.specs.values.foreach(_.walletRef ! ElectrumChainSync.ChainReorganized)
    goto(DISCONNECTED) using freshChain replying PoisonPill
  }

  electrum.pool ! ElectrumClient.AddStatusListener(self)

  startWith(DISCONNECTED, loadChain)

  var initialLocalTip: Int = 0
  var reportedTip: Int = 0

  when(DISCONNECTED) {
    case Event(_: ElectrumClient.ElectrumReady, blockchain) =>
      electrum.pool ! ElectrumClient.HeaderSubscription(self)
      goto(WAITING_FOR_TIP) using blockchain
  }

  when(WAITING_FOR_TIP) {
    case Event(response: ElectrumClient.HeaderSubscriptionResponse, blockchain)
      if blockchain.isReorg(response.height, response.header, electrum.params.headerDb) =>
      resetAfterReorg

    case Event(response: ElectrumClient.HeaderSubscriptionResponse, blockchain)
      if response.height < blockchain.height =>
      goto(DISCONNECTED) replying PoisonPill

    case Event(response: ElectrumClient.HeaderSubscriptionResponse, blockchain) if blockchain.bestchain.isEmpty =>
      electrum.pool ! ElectrumClient.GetHeaders(blockchain.checkpoints.size * RETARGETING_PERIOD, RETARGETING_PERIOD)
      initialLocalTip = blockchain.checkpoints.size * RETARGETING_PERIOD
      reportedTip = response.height
      goto(SYNCING)

    case Event(response: ElectrumClient.HeaderSubscriptionResponse, blockchain) if response.header == blockchain.tip.header =>
      context.system.eventStream publish ElectrumChainSync.ChainSyncEnded(blockchain.height)
      context.system.eventStream publish blockchain
      goto(RUNNING)

    case Event(response: ElectrumClient.HeaderSubscriptionResponse, blockchain) =>
      electrum.pool ! ElectrumClient.GetHeaders(blockchain.tip.height + 1, RETARGETING_PERIOD)
      initialLocalTip = blockchain.height
      reportedTip = response.height
      goto(SYNCING)
  }

  when(SYNCING) {
    case Event(ElectrumClient.GetHeadersResponse(start, header0 +: _, _), blockchain)
      if blockchain.isReorg(start, header0, electrum.params.headerDb) =>
      resetAfterReorg

    case Event(response: ElectrumClient.GetHeadersResponse, blockchain) if response.headers.isEmpty =>
      context.system.eventStream publish ElectrumChainSync.ChainSyncEnded(blockchain.height)
      context.system.eventStream publish blockchain
      goto(RUNNING)

    case Event(ElectrumClient.GetHeadersResponse(start, headers, _), blockchain) =>
      val blockchain1Try = Try apply Blockchain.addHeaders(blockchain, start, headers)

      blockchain1Try match {
        case Success(blockchain1) =>
          val (blockchain2, chunks) = Blockchain.optimize(blockchain1)
          if (chunks.nonEmpty) electrum.params.headerDb.addHeaders(chunks.map(_.header), chunks.head.height)
          context.system.eventStream publish ElectrumChainSync.ChainSyncing(initialLocalTip, blockchain.height, reportedTip)
          electrum.pool ! ElectrumClient.GetHeaders(blockchain2.tip.height + 1, RETARGETING_PERIOD)
          goto(SYNCING) using blockchain2

        case Failure(error) =>
          log.error(s"Electrum peer sent bad headers: $error")
          goto(DISCONNECTED) replying PoisonPill
      }

    case Event(ElectrumClient.HeaderSubscriptionResponse(height, header), _) =>
      log.debug(s"Ignoring header $header at $height while syncing")
      stay
  }

  when(RUNNING) {
    case Event(ElectrumClient.HeaderSubscriptionResponse(height, header), blockchain)
      if blockchain.isReorg(height, header, electrum.params.headerDb) =>
      resetAfterReorg

    case Event(ElectrumClient.HeaderSubscriptionResponse(height, header), blockchain) if blockchain.tip.header != header =>
      val difficultyOk = Blockchain.getDifficulty(blockchain, height, electrum.params.headerDb).forall(header.bits.==)
      val blockchain1Try = Try apply Blockchain.addHeader(blockchain, height, header)

      blockchain1Try match {
        case Success(blockchain1) if difficultyOk =>
          val (blockchain2, chunks) = Blockchain.optimize(blockchain1)
          if (chunks.nonEmpty) electrum.params.headerDb.addHeaders(chunks.map(_.header), chunks.head.height)
          context.system.eventStream publish blockchain2
          stay using blockchain2

        case _ =>
          stay replying PoisonPill
      }

    case Event(ElectrumClient.GetHeadersResponse(start, headers, _), blockchain) =>
      val blockchain1Try = Try apply Blockchain.addHeaders(blockchain, start, headers)

      blockchain1Try match {
        case Success(blockchain1) =>
          electrum.params.headerDb.addHeaders(headers, start)
          context.system.eventStream publish blockchain1
          stay using blockchain1

        case _ =>
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
