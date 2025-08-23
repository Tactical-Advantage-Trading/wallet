package immortan.utils

import java.net.InetSocketAddress

import akka.actor.Actor
import fr.acinq.bitcoin.ByteVector32
import fr.acinq.eclair.blockchain.electrum.ElectrumChainSync
import fr.acinq.eclair.blockchain.electrum.ElectrumClient._
import fr.acinq.eclair.blockchain.electrum.ElectrumWallet._
import immortan.Tools.none


object WalletEventsCatcher {
  case class Remove(listener: WalletEventsListener)
}

class WalletEventsCatcher extends Actor {
  // Not using a set to ensure insertion order
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

    case event: WalletReady => for (lst <- listeners) lst.onWalletReady(event)

    case event: TransactionReceived =>
      val event1 = transactionReceived.get(event.tx.txid).map(_ merge event).getOrElse(event)
      transactionReceived = transactionReceived.updated(event.tx.txid, event1)
      for (lst <- listeners) lst.onTransactionReceived(event1)

    case event: ElectrumReady => for (lst <- listeners) lst.onChainMasterSelected(event.serverAddress)

    case ElectrumDisconnected => for (lst <- listeners) lst.onChainDisconnected

    case event: ElectrumChainSync.ChainSyncing => for (lst <- listeners) lst.onChainSyncing(event.initialLocalTip, event.localTip, event.remoteTip)

    case event: ElectrumChainSync.ChainSyncEnded => for (lst <- listeners) lst.onChainSyncEnded(event.localTip)
  }
}

class WalletEventsListener {
  def onWalletReady(event: WalletReady): Unit = none

  def onTransactionReceived(event: TransactionReceived): Unit = none

  def onChainMasterSelected(event: InetSocketAddress): Unit = none

  def onChainDisconnected: Unit = none

  def onChainSyncing(start: Int, current: Int, max: Int): Unit = none

  def onChainSyncEnded(localTip: Int): Unit = none
}
