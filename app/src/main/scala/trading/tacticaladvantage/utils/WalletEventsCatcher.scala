package trading.tacticaladvantage.utils

import akka.actor.Actor
import fr.acinq.bitcoin.ByteVector32
import fr.acinq.eclair.blockchain.electrum.{Blockchain, ElectrumChainSync}
import fr.acinq.eclair.blockchain.electrum.ElectrumClient._
import fr.acinq.eclair.blockchain.electrum.ElectrumWallet._
import trading.tacticaladvantage.Tools.none
import java.net.InetSocketAddress


object WalletEventsCatcher {
  case class Remove(listener: WalletEventsListener)
}

class WalletEventsCatcher(netId: Int) extends Actor {
  // Not using a set here to ensure insertion order
  var listeners: List[WalletEventsListener] = Nil

  // When both sending and reciving, a single tx may affect many wallets
  // if that happens we will receive many successive TransactionReceived events
  // our job here is to merge all of them and provide a holistic event downstream
  var transactionReceived: Map[ByteVector32, TransactionReceived] = Map.empty

  context.system.eventStream.subscribe(channel = classOf[WalletEvent], subscriber = self)
  context.system.eventStream.subscribe(channel = classOf[ElectrumEvent], subscriber = self)
  context.system.eventStream.subscribe(channel = classOf[ElectrumChainSync.ChainSyncing], subscriber = self)
  context.system.eventStream.subscribe(channel = classOf[ElectrumChainSync.ChainSyncEnded], subscriber = self)

  override def receive: Receive = {
    case listener: WalletEventsListener => listeners = (listeners :+ listener).distinct

    case WalletEventsCatcher.Remove(listener) => listeners = listeners diff List(listener)

    case event: WalletReady => for (lst <- listeners) lst.onWalletReady(netId, event)

    case event: TransactionReceived =>
      val event1 = transactionReceived.get(event.tx.txid).map(_ merge event).getOrElse(event)
      transactionReceived = transactionReceived.updated(event.tx.txid, event1)
      for (lst <- listeners) lst.onTransactionReceived(netId, event1)

    case event: ElectrumReady => for (lst <- listeners) lst.onChainMasterSelected(netId, event.serverAddress)

    case ElectrumDisconnected => for (lst <- listeners) lst.onChainDisconnected(netId)

    case event: ElectrumChainSync.ChainSyncing => for (lst <- listeners) lst.onChainSyncing(netId, event.initialLocalTip, event.localTip, event.remoteTip)

    case event: ElectrumChainSync.ChainSyncEnded => for (lst <- listeners) lst.onChainSyncEnded(netId, event.localTip)
  }
}

class WalletEventsListener {
  def onWalletReady(netId: Int, event: WalletReady): Unit = none

  def onTransactionReceived(netId: Int, event: TransactionReceived): Unit = none

  def onChainMasterSelected(netId: Int, event: InetSocketAddress): Unit = none

  def onChainDisconnected(netId: Int): Unit = none

  def onChainSyncing(netId: Int, start: Int, current: Int, max: Int): Unit = none

  def onChainSyncEnded(netId: Int, localTip: Blockchain.BlockIndex): Unit = none
}
