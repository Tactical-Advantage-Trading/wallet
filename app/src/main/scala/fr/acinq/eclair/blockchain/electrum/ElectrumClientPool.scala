package fr.acinq.eclair.blockchain.electrum

import java.io.InputStream
import java.net.InetSocketAddress
import akka.actor.{Actor, ActorRef, FSM, OneForOneStrategy, Props, SupervisorStrategy, Terminated}
import fr.acinq.bitcoin.{Block, BlockHeader, ByteVector32}
import fr.acinq.eclair.blockchain.electrum.ElectrumClient.SSL
import fr.acinq.eclair.blockchain.electrum.ElectrumClientPool._
import org.json4s.JsonAST.{JObject, JString}
import org.json4s.native.JsonMethods

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import scala.util.Random


class ElectrumClientPool(chainHash: ByteVector32)(implicit val ec: ExecutionContext) extends Actor with FSM[State, Data] {
  val serverAddresses: Set[ElectrumServerAddress] = loadFromChainHash(chainHash)
  val addresses = collection.mutable.Map.empty[ActorRef, InetSocketAddress]
  val statusListeners = collection.mutable.HashSet.empty[ActorRef]

  // Always stop Electrum clients when there's a problem, we will automatically reconnect to another client
  override def supervisorStrategy: SupervisorStrategy = OneForOneStrategy(loggingEnabled = true) {
    case _ => SupervisorStrategy.stop
  }

  startWith(Disconnected, DisconnectedData)

  when(Disconnected) {
    case Event(ElectrumClient.ElectrumReady(height, tip, _), _) if addresses.contains(sender) =>
      sender ! ElectrumClient.HeaderSubscription(self)
      handleHeader(sender, height, tip, None)

    case Event(ElectrumClient.AddStatusListener(listener), _) =>
      statusListeners += listener
      stay

    case Event(Terminated(actor), _) =>
      context.system.scheduler.scheduleOnce(5.seconds, self, Connect)
      addresses -= actor
      stay
  }

  when(Connected) {
    case Event(ElectrumClient.ElectrumReady(height, tip, _), d: ConnectedData) if addresses.contains(sender) =>
      sender ! ElectrumClient.HeaderSubscription(self)
      handleHeader(sender, height, tip, Some(d))

    case Event(ElectrumClient.HeaderSubscriptionResponse(height, tip), d: ConnectedData) if addresses.contains(sender) =>
      handleHeader(sender, height, tip, Some(d))

    case Event(request: ElectrumClient.Request, d: ConnectedData) =>
      d.master forward request
      stay

    case Event(ElectrumClient.AddStatusListener(listener), d: ConnectedData) if addresses.contains(d.master) =>
      statusListeners += listener
      val (height, tip) = d.tips(d.master)
      listener ! ElectrumClient.ElectrumReady(height, tip, addresses(d.master))
      stay

    case Event(Terminated(actor), d: ConnectedData) =>
      val address = addresses(actor)
      val tips1 = d.tips - actor

      context.system.scheduler.scheduleOnce(5.seconds, self, Connect)
      addresses -= actor

      if (tips1.isEmpty) {
        log.info("lost connection to {}, no active connections left", address)
        goto(Disconnected) using DisconnectedData // no more connections
      } else if (d.master != actor) {
        log.debug("lost connection to {}, we still have our master server", address)
        stay using d.copy(tips = tips1) // we don't care, this wasn't our master
      } else {
        log.info("lost connection to our master server {}", address)
        // we choose next best candidate as master
        val tips1 = d.tips - actor
        val (bestClient, bestTip) = tips1.toSeq.maxBy(_._2._1)
        handleHeader(bestClient, bestTip._1, bestTip._2, Some(d.copy(tips = tips1)))
      }
  }

  whenUnhandled {
    case Event(InitConnect, _) =>
      val connections = Math.min(3, serverAddresses.size)
      (0 until connections).foreach(_ => self ! Connect)
      stay

    case Event(Connect, _) =>
      pickAddress(serverAddresses, addresses.values.toSet) foreach { esa =>
        val resolved = new InetSocketAddress(esa.address.getHostName, esa.address.getPort)
        val client = context actorOf Props(classOf[ElectrumClient], resolved, esa.ssl, ec)
        client ! ElectrumClient.AddStatusListener(self)
        addresses += Tuple2(client, esa.address)
        context watch client
      }

      stay

    case Event(ElectrumClient.ElectrumDisconnected, _) =>
      // Ignored, we rely on Terminated messages to detect disconnections
      stay
  }

  onTransition {
    case Connected -> Disconnected =>
      statusListeners.foreach(_ ! ElectrumClient.ElectrumDisconnected)
      context.system.eventStream.publish(ElectrumClient.ElectrumDisconnected)
  }

  initialize

  private def handleHeader(connection: ActorRef, height: Int, tip: BlockHeader, data: Option[ConnectedData] = None) = {
    val remoteAddress = addresses(connection)

    data match {
      case None =>
        // as soon as we have a connection to an electrum server, we select it as master
        log.info("selecting master {} at {}", remoteAddress, tip)
        statusListeners.foreach(_ ! ElectrumClient.ElectrumReady(height, tip, remoteAddress))
        context.system.eventStream.publish(ElectrumClient.ElectrumReady(height, tip, remoteAddress))
        goto(Connected) using ConnectedData(connection, Map(connection -> (height, tip)))
      case Some(d) if connection != d.master && height > d.blockHeight + 2 =>
        // we only switch to a new master if there is a significant difference with our current master, because
        // we don't want to switch to a new master every time a new block arrives (some servers will be notified before others)
        // we check that the current connection is not our master because on regtest when you generate several blocks at once
        // (and maybe on testnet in some pathological cases where there's a block every second) it may seen like our master
        // skipped a block and is suddenly at height + 2
        log.info("switching to master {} at {}", remoteAddress, tip)
        // we've switched to a new master, treat this as a disconnection/reconnection
        // so users (wallet, watcher, ...) will reset their subscriptions
        statusListeners.foreach(_ ! ElectrumClient.ElectrumDisconnected)
        context.system.eventStream.publish(ElectrumClient.ElectrumDisconnected)
        statusListeners.foreach(_ ! ElectrumClient.ElectrumReady(height, tip, remoteAddress))
        context.system.eventStream.publish(ElectrumClient.ElectrumReady(height, tip, remoteAddress))
        goto(Connected) using d.copy(master = connection, tips = d.tips + (connection -> (height, tip)))
      case Some(d) =>
        log.debug("received tip {} from {} at {}", tip, remoteAddress, height)
        stay using d.copy(tips = d.tips + (connection -> (height, tip)))
    }
  }
}

object ElectrumClientPool {
  case class ElectrumServerAddress(address: InetSocketAddress, ssl: SSL)

  var loadFromChainHash: ByteVector32 => Set[ElectrumServerAddress] = {
    case Block.LivenetGenesisBlock.hash => readServerAddresses(classOf[ElectrumServerAddress] getResourceAsStream "/electrum/servers_mainnet.json")
    case Block.TestnetGenesisBlock.hash => readServerAddresses(classOf[ElectrumServerAddress] getResourceAsStream "/electrum/servers_testnet.json")
    case Block.RegtestGenesisBlock.hash => readServerAddresses(classOf[ElectrumServerAddress] getResourceAsStream "/electrum/servers_regtest.json")
    case _ => throw new RuntimeException
  }

  def readServerAddresses(stream: InputStream): Set[ElectrumServerAddress] = try {
    val JObject(values) = JsonMethods.parse(stream)

    for (Tuple2(name, fields) <- values.toSet) yield {
      val port = (fields \ "s").asInstanceOf[JString].s.toInt
      val address = InetSocketAddress.createUnresolved(name, port)
      ElectrumServerAddress(address, SSL.LOOSE)
    }

  } finally {
    stream.close
  }

  def pickAddress(serverAddresses: Set[ElectrumServerAddress], usedAddresses: Set[InetSocketAddress] = Set.empty): Option[ElectrumServerAddress] =
    Random.shuffle(serverAddresses.filterNot(serverAddress => usedAddresses contains serverAddress.address).toSeq).headOption

  sealed trait State
  case object Disconnected extends State
  case object Connected extends State

  sealed trait Data
  case object DisconnectedData extends Data

  type TipAndHeader = (Int, BlockHeader)
  type ActorTipAndHeader = Map[ActorRef, TipAndHeader]

  case class ConnectedData(master: ActorRef, tips: ActorTipAndHeader) extends Data {
    def blockHeight: Int = tips.get(master).map(_._1).getOrElse(0)
  }

  case object Connect
  case object InitConnect
}
