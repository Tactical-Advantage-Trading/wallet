package fr.acinq.eclair.blockchain.electrum

import akka.actor.{FSM, PoisonPill}
import fr.acinq.bitcoin.ByteVector32
import fr.acinq.eclair.blockchain.electrum.ElectrumWallet.{SYNCING, WAITING_TIP}
import fr.acinq.eclair.blockchain.electrum.SidechainHashSearch._
import fr.acinq.eclair.wire.CommonCodecs.bytes32
import scodec.Codec
import scodec.codecs.{int32, listOfN, uint16}
import trading.tacticaladvantage.Tools.Any2Some
import scala.concurrent.duration.DurationInt


object SidechainHashSearch {
  final val BITASSETS_NUM = 4
  final val THUNDER_NUM = 9

  type MainHeight2SideHash = (Int, ByteVector32)
  type SideChainNum2Info = Map[Int, MainHeight2SideHash]
  case class HashFound(sideNum: Int, sideHash: ByteVector32)

  sealed trait Data
  case class Waiting(state: SideChainNum2Info) extends Data
  case class Synching(sidesLeft: Set[Int], state: SideChainNum2Info, blockchain: Blockchain, stopHeight: Int,
                      newSyncEnded: Option[ChainSyncEnded] = None, txids: Map[ByteVector32, Int] = Map.empty) extends Data {
    def withTxid(msg: ElectrumClient.GetTransactionIdFromPositionResponse): Synching = copy(txids = Map(msg.txid -> msg.height) ++ txids)
  }

  private val singleEntryCodec = int32 ~ (int32 ~ bytes32)
  val codec: Codec[SideChainNum2Info] = listOfN(uint16, singleEntryCodec).xmap(_.toMap, _.toList)
  val searchSet = Set(BITASSETS_NUM, THUNDER_NUM)
}

class SidechainHashSearch(electrum: Electrum) extends FSM[ElectrumWallet.State, Data] {
  context.system.eventStream.subscribe(channel = classOf[ChainSyncEvent], subscriber = self)
  electrum.pool ! ElectrumClient.AddStatusListener(self)

  {
    val state = electrum.params.dataDb.tryGetSideHashes
    val waiting = Waiting(state getOrElse Map.empty)
    startWith(WAITING_TIP, waiting)
  }

  when(WAITING_TIP) {
    case Event(ChainSyncEnded(oldHeight, chain), data: Waiting) if chain.tip.height == oldHeight =>
      data.state.mapValues(_._2).map(HashFound.tupled).foreach(context.system.eventStream.publish)
      stay

    case Event(ChainSyncEnded(oldHeight, chain), data: Waiting) if chain.tip.height > oldHeight =>
      val data1 = Synching(searchSet, data.state, chain, stopHeight = oldHeight max chain.tip.height - 72)
      goto(SYNCING) using data1 sending chain.tip.height
  }

  when(SYNCING, stateTimeout = 1.minute) {
    case Event(height: Int, data: Synching) if data.sidesLeft.isEmpty || height < data.stopHeight =>
      goto(WAITING_TIP) using Waiting(data.state) sendingIfSome data.newSyncEnded

    case Event(height: Int, _: Synching) =>
      electrum.pool ! ElectrumClient.GetTransactionIdFromPosition(height)
      stay

    case Event(msg: ElectrumClient.GetTransactionIdFromPositionResponse, data: Synching) =>
      data.blockchain.getHeader(msg.height) collectFirst {
        case header if header.hashMerkleRoot == msg.hashMerkleRoot =>
          electrum.pool ! ElectrumClient.GetTransaction(msg.txid)
          stay using data.withTxid(msg)
      } getOrElse {
        // Not much we can do, hope it will resolve with a different peer
        goto(WAITING_TIP) using Waiting(data.state) replying PoisonPill
      }

    case Event(msg: ElectrumClient.GetTransactionResponse, data: Synching) =>
      data.txids.get(msg.tx.txid) map { l1Height =>
        val sideNum2SideHash = msg.sidechainBlockHashes.toMap.filterKeys(data.sidesLeft.contains)
        val state1 = data.state ++ sideNum2SideHash.mapValues(sideChainHash => l1Height -> sideChainHash)
        val data1 = data.copy(sidesLeft = data.sidesLeft -- sideNum2SideHash.keys, state = state1, txids = data.txids - msg.tx.txid)
        sideNum2SideHash.map(HashFound.tupled).foreach(context.system.eventStream.publish)
        if (sideNum2SideHash.nonEmpty) electrum.params.dataDb.putSideHashes(state1)
        stay using data1 sending l1Height - 1
      } getOrElse stay

    case Event(StateTimeout, data: Synching) =>
      goto(WAITING_TIP) using Waiting(data.state)
  }

  whenUnhandled {
    case Event(ChainSyncEnded(oldHeight, chain), data: Synching) if chain.tip.height > oldHeight =>
      val oldHeight1 = data.newSyncEnded.map(_.oldLocalHeight).getOrElse(Int.MaxValue).min(oldHeight)
      stay using data.copy(newSyncEnded = ChainSyncEnded(oldHeight1, chain).asSome)

    case Event(ElectrumClient.ElectrumDisconnected, data: Synching) =>
      goto(WAITING_TIP) using Waiting(data.state)

    case Event(ChainReorganized, _) =>
      electrum.params.dataDb.putSideHashes(Map.empty)
      goto(WAITING_TIP) using Waiting(Map.empty)
  }

  implicit class MyState(state: State) {
    def sendingIfSome(msg: Option[Any] = None) =
      msg.map(sending).getOrElse(state)

    def sending(msg: Any): State = {
      self ! msg
      state
    }
  }

  initialize
}
