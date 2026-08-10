package fr.acinq.eclair.blockchain.electrum

import akka.actor.{Actor, FSM, PoisonPill}
import fr.acinq.bitcoin.{BlockHeader, ByteVector32}
import fr.acinq.eclair.blockchain.electrum.ElectrumWallet.{DISCONNECTED, SYNCING, WAITING_TIP}
import fr.acinq.eclair.wire.CommonCodecs.bytes32
import scodec.Codec
import scodec.codecs.{listOfN, uint16}
import trading.tacticaladvantage.Tools.{Any2Some, IterableOfTuple2}
import trading.tacticaladvantage.sqlite.SQLiteData
import trading.tacticaladvantage.utils.Rx
import fr.acinq.eclair.blockchain.electrum.SidechainHashSearch._


object SidechainHashSearch {
  final val BITASSETS_NUM = 4
  final val THUNDER_NUM = 9

  type MainHeight2SideHash = (Int, ByteVector32)
  type SideChainNum2Info = Map[Int, MainHeight2SideHash]
  case class HashFound(sideChainNum: Int, hash: ByteVector32)

  sealed trait Data
  case class Waiting(state: SideChainNum2Info) extends Data
  case class Synching(sidesLeft: Set[Int], state: SideChainNum2Info, blockchain: Blockchain, stopHeight: Int,
                      newSyncEnded: Option[ChainSyncEnded] = None, heights: Map[Int, ByteVector32] = Map.empty,
                      txids: Set[ByteVector32] = Set.empty) extends Data

  private val singleEntryCodec = uint16 ~ (uint16 ~ bytes32)
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
      data.state.values.map(SidechainHashSearch.HashFound.tupled).foreach(context.system.eventStream.publish)
      stay

    case Event(ChainSyncEnded(oldHeight, chain), data: Waiting) if chain.tip.height > oldHeight =>
      val data1 = Synching(searchSet, data.state, chain, stopHeight = oldHeight max chain.tip.height - 72)
      goto(SYNCING) using data1 sending chain.tip.height
  }

  when(SYNCING) {
    case Event(height: Int, data) =>
      stay

//    case Event(bi: Blockchain.BlockIndex, data) =>
//      // we should start descent from bs.height as it is guaranteed to be higher or equal to state.highestCheckedTip
//      // we decide to stop based on depth depending on state.highestCheckedTip - last call depth
//      // on sync end, we request blockchain and go to WAITING_TIP, in case if new blocks have arrived
//      // on bad response we retain the traversed state so far and go to DISCONNECTED
//
//      electrum.pool ! ElectrumClient.GetTransactionIdFromPosition(bi.height)
//      stay using state.copy(heights = state.heights + bi.heightMerkleId)
//
//    case Event(data: ElectrumClient.GetTransactionIdFromPositionResponse, state: SyncState) if state.heights.contains(data.height) =>
//      // We check Merkle inclusion proof only if this server reply is at all expected, otherwise we just do nothing
//      state.copy(heights = state.heights - data.height) match {
//        case state1 if state.heights(data.height) == data.hashMerkleRoot =>
//          electrum.pool ! ElectrumClient.GetTransaction(data.txid)
//          stay using state1.copy(txids = state.txids + data.txid)
//        case state1 =>
//          // Server replied with incorrect transaction
//          goto(WAITING_TIP) using state1 replying PoisonPill
//      }
//
//    case Event(data: ElectrumClient.GetTransactionResponse, state: SyncState) if state.txids.contains(data.tx.txid) =>
  }

  whenUnhandled {
    case Event(ChainSyncEnded(oldHeight, chain), data: Synching) if chain.tip.height > oldHeight =>
      val oldHeight1 = data.newSyncEnded.map(_.oldLocalHeight).getOrElse(Int.MaxValue).min(oldHeight)
      stay using data.copy(newSyncEnded = ChainSyncEnded(oldHeight1, chain).asSome)

    case Event(ChainReorganized, _) =>
      electrum.params.dataDb.putSideHashes(Map.empty)
      goto(WAITING_TIP) using Waiting(Map.empty)
  }

  implicit class MyState(state: State) {
    def sending(msg: Any): State = {
      self ! msg
      state
    }
  }

//  def main(heights: Map[Int, ByteVector32], txids: Set[ByteVector32], sidesLeft: Set[Int], hashes: SidechainHashSearch.SideNum2LastHash): Receive = {
//
//    case ChainSyncEnded(l1Tip) if hashes.values.firstItems.max >= l1Tip.height =>
//      hashes.values.map(SidechainHashSearch.HashFound.tupled).foreach(context.system.eventStream.publish)
//
//    case ChainSyncEnded(l1Tip) =>
//      electrum.pool ! ElectrumClient.GetTransactionIdFromPosition(l1Tip.height)
//      context become main(heights + (l1Tip.height, l1Tip.header.hashMerkleRoot), txids, sidesLeft, hashes)
//
//    case ChainReorganized(newHeight, header) =>
//      electrum.pool ! ElectrumClient.GetTransactionIdFromPosition(newHeight)
//      context become main(Map(newHeight -> header.hashMerkleRoot), Set.empty, sideChains, Map.empty)
//      electrum.params.dataDb.putSideHeaders(Map.empty)
//
//    case data: ElectrumClient.GetTransactionIdFromPositionResponse if heights.contains(data.height) =>
//      if (heights(data.height) == data.hashMerkleRoot) {
//        context become main(heights - data.height, txids + data.txid, sidesLeft, hashes)
//        electrum.pool ! ElectrumClient.GetTransaction(data.txid)
//      } else {
//        sender ! PoisonPill
//        Rx.delay(2000).foreach { _ =>
//          // Kill errouneous server right away and then ask another one
//          // Give client pool some time to switch master node before asking
//          electrum.pool ! ElectrumClient.GetTransactionIdFromPosition(data.height)
//        }
//      }
//
//    case data: ElectrumClient.GetTransactionResponse if txids.contains(data.tx.txid) =>
//      val relevantHashes = data.sidechainBlockHashes.toMap.filterKeys(sideChains.contains)
//      relevantHashes.map(SidechainHashSearch.HashFound.tupled).foreach(context.system.eventStream.publish)
//      context become main(heights, txids - data.tx.txid, sidesLeft -- relevantHashes.keys, relevantHashes + hashes)
//      // TODO: now, if we have sidechains to check left, their hashes may be in previous blocks, so we need to start descending one block at a time, how?
//  }
//
//  override def receive: Receive = {
//    val hashes = electrum.params.dataDb.tryGetSideHeaders.getOrElse(Map.empty)
//    main(Map.empty, Set.empty, sideChains, hashes)
//  }
}
