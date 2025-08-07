package fr.acinq.eclair.blockchain.electrum

import java.net.{InetSocketAddress, SocketAddress}
import java.util

import akka.actor.{Actor, ActorRef, Cancellable, Stash, Terminated}
import fr.acinq.bitcoin._
import fr.acinq.eclair.blockchain.bitcoind.rpc.{Error, JsonRPCRequest, JsonRPCResponse}
import fr.acinq.eclair.blockchain.electrum.ElectrumClient._
import fr.acinq.eclair.blockchain.fee.FeeratePerKw
import io.netty.bootstrap.Bootstrap
import io.netty.buffer.PooledByteBufAllocator
import io.netty.channel._
import io.netty.channel.nio.NioEventLoopGroup
import io.netty.channel.socket.SocketChannel
import io.netty.channel.socket.nio.NioSocketChannel
import io.netty.handler.codec.string.{LineEncoder, StringDecoder}
import io.netty.handler.codec.{LineBasedFrameDecoder, MessageToMessageDecoder, MessageToMessageEncoder}
import io.netty.handler.proxy.Socks5ProxyHandler
import io.netty.handler.ssl.SslContextBuilder
import io.netty.handler.ssl.util.InsecureTrustManagerFactory
import io.netty.resolver.NoopAddressResolverGroup
import io.netty.util.CharsetUtil
import org.json4s.JsonAST._
import org.json4s.native.JsonMethods
import org.json4s.{JInt, JLong, JString}
import scodec.bits.ByteVector

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}


class ElectrumClient(serverAddress: InetSocketAddress, ssl: SSL)(implicit val ec: ExecutionContext) extends Actor with Stash {

  val b = new Bootstrap
  b channel classOf[NioSocketChannel]
  b group workerGroup

  b.option[java.lang.Boolean](ChannelOption.TCP_NODELAY, true)
  b.option[java.lang.Boolean](ChannelOption.SO_KEEPALIVE, true)
  b.option[java.lang.Integer](ChannelOption.CONNECT_TIMEOUT_MILLIS, 5000)
  b.option(ChannelOption.ALLOCATOR, PooledByteBufAllocator.DEFAULT)

  b handler new ChannelInitializer[SocketChannel] {
    override def initChannel(ch: SocketChannel): Unit = {
      if (ssl == SSL.LOOSE || serverAddress.getPort == 50002) {
        val sslCtx = SslContextBuilder.forClient.trustManager(InsecureTrustManagerFactory.INSTANCE).build
        ch.pipeline addLast sslCtx.newHandler(ch.alloc, serverAddress.getHostName, serverAddress.getPort)
      }

      // Inbound
      ch.pipeline addLast new LineBasedFrameDecoder(Int.MaxValue, true, true)
      ch.pipeline addLast new StringDecoder(CharsetUtil.UTF_8)
      ch.pipeline addLast new ElectrumResponseDecoder
      ch.pipeline addLast new ActorHandler(self)

      // Outbound
      ch.pipeline.addLast(new LineEncoder)
      ch.pipeline.addLast(new JsonRPCRequestEncoder)

      // Error handler
      ch.pipeline.addLast(new ExceptionHandler)

      ElectrumWallet.connectionProvider.proxyAddress.foreach { address =>
        // Optional proxy which must be the first handler
        val handler = new Socks5ProxyHandler(address)
        ch.pipeline.addFirst(handler)
      }
    }
  }

  val channelOpenFuture: ChannelFuture = b.connect(serverAddress.getHostName, serverAddress.getPort)

  if (ElectrumWallet.connectionProvider.proxyAddress.isDefined) b.resolver(NoopAddressResolverGroup.INSTANCE)

  channelOpenFuture addListeners new ChannelFutureListener {
    override def operationComplete(future: ChannelFuture): Unit = {
      if (!future.isSuccess) {
        self ! Close
      } else {
        future.channel.closeFuture addListener new ChannelFutureListener {
          override def operationComplete(future: ChannelFuture): Unit = self ! Close
        }
      }
    }
  }

  class ExceptionHandler extends ChannelDuplexHandler {
    override def connect(ctx: ChannelHandlerContext, remoteAddress: SocketAddress, localAddress: SocketAddress, promise: ChannelPromise): Unit = {
      val listener = new ChannelFutureListener { override def operationComplete(future: ChannelFuture): Unit = if (!future.isSuccess) self ! Close }
      ctx.connect(remoteAddress, localAddress, promise addListener listener)
    }

    override def write(ctx: ChannelHandlerContext, msg: scala.Any, promise: ChannelPromise): Unit = {
      val listener = new ChannelFutureListener { override def operationComplete(future: ChannelFuture): Unit = if (!future.isSuccess) self ! Close }
      ctx.write(msg, promise addListener listener)
    }

    override def exceptionCaught(ctx: ChannelHandlerContext, cause: Throwable): Unit = self ! Close
  }

  class ElectrumResponseDecoder extends MessageToMessageDecoder[String] {
    override def decode(ctx: ChannelHandlerContext, msg: String, out: util.List[AnyRef]): Unit = {
      val s = msg.asInstanceOf[String]
      val r = parseResponse(s)
      out.add(r)
    }
  }

  class JsonRPCRequestEncoder extends MessageToMessageEncoder[JsonRPCRequest] {
    override def encode(ctx: ChannelHandlerContext, request: JsonRPCRequest, out: util.List[AnyRef]): Unit = {
      import org.json4s.JsonDSL._
      import org.json4s._

      val json = ("method" -> request.method) ~ ("params" -> request.params.map {
        case s: String => new JString(s)
        case b: ByteVector32 => new JString(b.toHex)
        case f: FeeratePerKw => new JLong(f.toLong)
        case b: Boolean => new JBool(b)
        case t: Int => new JInt(t)
        case t: Long => new JLong(t)
        case t: Double => new JDouble(t)
      }) ~ ("id" -> request.id) ~ ("jsonrpc" -> request.jsonrpc)
      val serialized = JsonMethods.compact(JsonMethods.render(json))
      out.add(serialized)
    }
  }

  class ActorHandler(actor: ActorRef) extends ChannelInboundHandlerAdapter {
    override def channelActive(ctx: ChannelHandlerContext): Unit = actor ! ctx
    override def channelRead(ctx: ChannelHandlerContext, msg: Any): Unit = actor ! msg
  }

  type ActorSet = Set[ActorRef]
  var addressSubscriptions = Map.empty[String, ActorSet]
  var scriptHashSubscriptions = Map.empty[ByteVector32, ActorSet]
  val headerSubscriptions = collection.mutable.HashSet.empty[ActorRef]
  val statusListeners = collection.mutable.HashSet.empty[ActorRef]
  val version = ServerVersion(CLIENT_NAME, PROTOCOL_VERSION)
  var reqId = 0

  // We need to regularly send a ping in order not to get disconnected
  val pingTrigger: Cancellable = context.system.scheduler.schedule(30.seconds, 30.seconds, self, Ping)

  override def unhandled(message: Any): Unit = {
    message match {
      case Terminated(deadActor) =>
        addressSubscriptions = addressSubscriptions.mapValues(subscribers => subscribers - deadActor)
        scriptHashSubscriptions = scriptHashSubscriptions.mapValues(subscribers => subscribers - deadActor)
        statusListeners -= deadActor
        headerSubscriptions -= deadActor

      case RemoveStatusListener(actor) => statusListeners -= actor

      case Close =>
        statusListeners.foreach(_ ! ElectrumDisconnected)
        context.stop(self)

      case _ =>
        // Do nothing
    }
  }

  override def postStop: Unit = {
    pingTrigger.cancel
    super.postStop
  }

  def send(ctx: ChannelHandlerContext, request: Request): String = {
    val electrumRequestId = reqId.toString

    if (ctx.channel.isWritable) ctx.channel writeAndFlush makeRequest(request, electrumRequestId) else self ! Close

    reqId = reqId + 1
    electrumRequestId
  }

  def receive: Receive = disconnected

  def disconnected: Receive = {
    case ctx: ChannelHandlerContext =>
      send(ctx, version)
      context become waitingForVersion(ctx)

    case AddStatusListener(actor) => statusListeners += actor
  }

  def waitingForVersion(ctx: ChannelHandlerContext): Receive = {
    case Right(json: JsonRPCResponse) =>
      (parseJsonResponse(version, json): @unchecked) match {
        case _: ServerVersionResponse =>
          send(ctx, HeaderSubscription(self))
          headerSubscriptions += self
          context become waitingForTip(ctx)
        case _: ServerError =>
          self ! Close
      }

    case AddStatusListener(actor) => statusListeners += actor
  }

  def waitingForTip(ctx: ChannelHandlerContext): Receive = {
    case Right(json: JsonRPCResponse) =>
      val (height, header) = parseBlockHeader(json.result)
      statusListeners.foreach(_ ! ElectrumReady(height, header, serverAddress))
      context become connected(ctx, height, header, Map.empty)

    case AddStatusListener(actor) => statusListeners += actor
  }

  def connected(ctx: ChannelHandlerContext, height: Int, tip: BlockHeader, requests: Map[String, (Request, ActorRef)]): Receive = {
    case AddStatusListener(actor) =>
      statusListeners += actor
      actor ! ElectrumReady(height, tip, serverAddress)

    case HeaderSubscription(actor) =>
      headerSubscriptions += actor
      actor ! HeaderSubscriptionResponse(height, tip)
      context watch actor

    case request: Request =>
      val curReqId = send(ctx, request)
      request match {
        case AddressSubscription(address, actor) =>
          addressSubscriptions = addressSubscriptions.updated(address, addressSubscriptions.getOrElse(address, Set()) + actor)
          context watch actor
        case ScriptHashSubscription(scriptHash, actor) =>
          scriptHashSubscriptions = scriptHashSubscriptions.updated(scriptHash, scriptHashSubscriptions.getOrElse(scriptHash, Set()) + actor)
          context watch actor
        case _ => ()
      }
      context become connected(ctx, height, tip, requests + (curReqId -> (request, sender())))

    case Right(json: JsonRPCResponse) =>
      requests.get(json.id) match {
        case Some((request, requestor)) =>
          val response = parseJsonResponse(request, json)
          requestor ! response
        case None =>
      }
      context become connected(ctx, height, tip, requests - json.id)

    case Left(response: HeaderSubscriptionResponse) => headerSubscriptions.foreach(_ ! response)

    case Left(response: AddressSubscriptionResponse) => addressSubscriptions.get(response.address).foreach(listeners => listeners.foreach(_ ! response))

    case Left(response: ScriptHashSubscriptionResponse) => scriptHashSubscriptions.get(response.scriptHash).foreach(listeners => listeners.foreach(_ ! response))

    case HeaderSubscriptionResponse(height1, newtip) => context become connected(ctx, height1, newtip, requests)
  }
}

object ElectrumClient {
  val CLIENT_NAME = "3.3.6"
  val PROTOCOL_VERSION = "1.4"

  // this is expensive and shared with all clients
  val workerGroup = new NioEventLoopGroup

  def computeScriptHash(publicKeyScript: ByteVector): ByteVector32 = Crypto.sha256(publicKeyScript).reverse

  case class AddStatusListener(actor: ActorRef)
  case class RemoveStatusListener(actor: ActorRef)

  sealed trait Request
  sealed trait Response

  case class ServerVersion(clientName: String, protocolVersion: String) extends Request
  case class ServerVersionResponse(clientName: String, protocolVersion: String) extends Response

  case object Ping extends Request
  case object PingResponse extends Response

  case class GetAddressHistory(address: String) extends Request
  case class TransactionHistoryItem(height: Int, txHash: ByteVector32)
  case class GetAddressHistoryResponse(address: String, history: Seq[TransactionHistoryItem] = Nil) extends Response

  case class GetScriptHashHistory(scriptHash: ByteVector32) extends Request
  case class GetScriptHashHistoryResponse(scriptHash: ByteVector32, history: List[TransactionHistoryItem] = Nil) extends Response

  case class AddressListUnspent(address: String) extends Request
  case class UnspentItem(txHash: ByteVector32, txPos: Int, value: Long, height: Long) {
    lazy val outPoint = OutPoint(txHash.reverse, txPos)
  }
  case class AddressListUnspentResponse(address: String, unspents: Seq[UnspentItem] = Nil) extends Response

  case class ScriptHashListUnspent(scriptHash: ByteVector32) extends Request
  case class ScriptHashListUnspentResponse(scriptHash: ByteVector32, unspents: Seq[UnspentItem] = Nil) extends Response

  case class BroadcastTransaction(tx: Transaction) extends Request
  case class BroadcastTransactionResponse(tx: Transaction, error: Option[Error] = None) extends Response

  case class GetTransactionIdFromPosition(height: Int, txPos: Int, merkle: Boolean = false) extends Request
  case class GetTransactionIdFromPositionResponse(txid: ByteVector32, height: Int, txPos: Int, merkle: Seq[ByteVector32] = Nil) extends Response

  case class GetTransaction(txid: ByteVector32) extends Request
  case class GetTransactionResponse(tx: Transaction) extends Response

  case class GetHeader(height: Int) extends Request
  case class GetHeaderResponse(height: Int, header: BlockHeader) extends Response
  object GetHeaderResponse {
    def apply(t: (Int, BlockHeader)) = new GetHeaderResponse(t._1, t._2)
  }

  case class GetHeaders(startHeight: Int, count: Int, cpHeight: Int = 0) extends Request
  case class GetHeadersResponse(startHeight: Int, headers: Seq[BlockHeader], max: Int) extends Response

  case class GetMerkle(txid: ByteVector32, height: Int) extends Request
  case class GetMerkleResponse(txid: ByteVector32, merkle: List[ByteVector32], blockHeight: Int, pos: Int) extends Response {
    lazy val root: ByteVector32 = {
      @tailrec
      def loop(pos: Int, hashes: Seq[ByteVector32] = Nil): ByteVector32 = {
        if (hashes.length == 1) hashes.head
        else {
          val h = if (pos % 2 == 1) Crypto.hash256(hashes(1) ++ hashes.head) else Crypto.hash256(hashes.head ++ hashes(1))
          loop(pos / 2, h +: hashes.drop(2))
        }
      }
      loop(pos, txid.reverse +: merkle.map(_.reverse))
    }
  }

  case class AddressSubscription(address: String, actor: ActorRef) extends Request
  case class AddressSubscriptionResponse(address: String, status: String) extends Response

  case class ScriptHashSubscription(scriptHash: ByteVector32, actor: ActorRef) extends Request
  case class ScriptHashSubscriptionResponse(scriptHash: ByteVector32, status: String) extends Response

  case class HeaderSubscription(actor: ActorRef) extends Request
  case class HeaderSubscriptionResponse(height: Int, header: BlockHeader) extends Response
  object HeaderSubscriptionResponse {
    def apply(t: (Int, BlockHeader)) = new HeaderSubscriptionResponse(t._1, t._2)
  }

  case class Header(block_height: Long, version: Long, prev_block_hash: ByteVector32, merkle_root: ByteVector32, timestamp: Long, bits: Long, nonce: Long)

  object Header {
    def makeHeader(height: Long, header: BlockHeader) = ElectrumClient.Header(height, header.version,
      header.hashPreviousBlock.reverse, header.hashMerkleRoot.reverse, header.time, header.bits, header.nonce)

    val RegtestGenesisHeader: Header = makeHeader(0, Block.RegtestGenesisBlock.header)
    val TestnetGenesisHeader: Header = makeHeader(0, Block.TestnetGenesisBlock.header)
    val LivenetGenesisHeader: Header = makeHeader(0, Block.LivenetGenesisBlock.header)
  }

  case class TransactionHistory(history: Seq[TransactionHistoryItem]) extends Response

  case class AddressStatus(address: String, status: String) extends Response

  case class ServerError(request: Request, error: Error) extends Response

  sealed trait ElectrumEvent
  case class ElectrumReady(height: Int, tip: BlockHeader, serverAddress: InetSocketAddress) extends ElectrumEvent
  case object ElectrumDisconnected extends ElectrumEvent

  sealed trait SSL

  object SSL {
    case object DECIDE extends SSL
    case object LOOSE extends SSL
  }

  case object Close

  def parseResponse(input: String): Either[Response, JsonRPCResponse] = {
    val json = JsonMethods.parse(new String(input))
    json \ "method" match {
      case JString(method) =>
        // this is a jsonrpc request, i.e. a subscription response
        val JArray(params) = json \ "params"
        Left(((method, params): @unchecked) match {
          case ("blockchain.headers.subscribe", header :: Nil) => HeaderSubscriptionResponse(parseBlockHeader(header))
          case ("blockchain.address.subscribe", JString(address) :: JNull :: Nil) => AddressSubscriptionResponse(address, "")
          case ("blockchain.address.subscribe", JString(address) :: JString(status) :: Nil) => AddressSubscriptionResponse(address, status)
          case ("blockchain.scripthash.subscribe", JString(scriptHashHex) :: JNull :: Nil) => ScriptHashSubscriptionResponse(ByteVector32.fromValidHex(scriptHashHex), "")
          case ("blockchain.scripthash.subscribe", JString(scriptHashHex) :: JString(status) :: Nil) => ScriptHashSubscriptionResponse(ByteVector32.fromValidHex(scriptHashHex), status)
        })
      case _ => Right(parseJsonRpcResponse(json))
    }
  }

  def parseJsonRpcResponse(json: JValue): JsonRPCResponse = {
    val result = json \ "result"
    val error = json \ "error" match {
      case JNull => None
      case JNothing => None
      case other =>
        val message = other \ "message" match {
          case JString(value) => value
          case _ => ""
        }
        val code = other \ " code" match {
          case JInt(value) => value.intValue
          case JLong(value) => value.intValue
          case _ => 0
        }
        Some(Error(code, message))
    }
    val id = json \ "id" match {
      case JString(value) => value
      case JInt(value) => value.toString()
      case JLong(value) => value.toString
      case _ => ""
    }
    JsonRPCResponse(result, error, id)
  }

  def longField(jvalue: JValue, field: String): Long = (jvalue \ field: @unchecked) match {
    case JLong(value) => value.longValue
    case JInt(value) => value.longValue
  }

  def intField(jvalue: JValue, field: String): Int = (jvalue \ field: @unchecked) match {
    case JLong(value) => value.intValue
    case JInt(value) => value.intValue
  }

  def parseBlockHeader(json: JValue): (Int, BlockHeader) = {
    val height = intField(json, "height")
    val JString(hex) = json \ "hex"
    (height, BlockHeader.read(hex))
  }

  def makeRequest(request: Request, reqId: String): JsonRPCRequest = request match {
    case ServerVersion(clientName, protocolVersion) => JsonRPCRequest(id = reqId, method = "server.version", params = clientName :: protocolVersion :: Nil)
    case Ping => JsonRPCRequest(id = reqId, method = "server.ping", params = Nil)
    case GetAddressHistory(address) => JsonRPCRequest(id = reqId, method = "blockchain.address.get_history", params = address :: Nil)
    case GetScriptHashHistory(scripthash) => JsonRPCRequest(id = reqId, method = "blockchain.scripthash.get_history", params = scripthash.toHex :: Nil)
    case AddressListUnspent(address) => JsonRPCRequest(id = reqId, method = "blockchain.address.listunspent", params = address :: Nil)
    case ScriptHashListUnspent(scripthash) => JsonRPCRequest(id = reqId, method = "blockchain.scripthash.listunspent", params = scripthash.toHex :: Nil)
    case AddressSubscription(address, _) => JsonRPCRequest(id = reqId, method = "blockchain.address.subscribe", params = address :: Nil)
    case ScriptHashSubscription(scriptHash, _) => JsonRPCRequest(id = reqId, method = "blockchain.scripthash.subscribe", params = scriptHash.toString() :: Nil)
    case BroadcastTransaction(tx) => JsonRPCRequest(id = reqId, method = "blockchain.transaction.broadcast", params = Transaction.write(tx).toHex :: Nil)
    case GetTransactionIdFromPosition(height, tx_pos, merkle) => JsonRPCRequest(id = reqId, method = "blockchain.transaction.id_from_pos", params = height :: tx_pos :: merkle :: Nil)
    case GetTransaction(txid) => JsonRPCRequest(id = reqId, method = "blockchain.transaction.get", params = txid :: Nil)
    case HeaderSubscription(_) => JsonRPCRequest(id = reqId, method = "blockchain.headers.subscribe", params = Nil)
    case GetHeader(height) => JsonRPCRequest(id = reqId, method = "blockchain.block.header", params = height :: Nil)
    case GetHeaders(start_height, count, _) => JsonRPCRequest(id = reqId, method = "blockchain.block.headers", params = start_height :: count :: Nil)
    case GetMerkle(txid, height) => JsonRPCRequest(id = reqId, method = "blockchain.transaction.get_merkle", params = txid :: height :: Nil)
  }

  def parseJsonResponse(request: Request, json: JsonRPCResponse): Response = {
    json.error match {
      case Some(error) => (request: @unchecked) match {
        case BroadcastTransaction(tx) => BroadcastTransactionResponse(tx, Some(error)) // for this request type, error are considered a "normal" response
        case _ => ServerError(request, error)
      }
      case None => (request: @unchecked) match {
        case _: ServerVersion =>
          val JArray(jitems) = json.result
          val JString(clientName) = jitems.head
          val JString(protocolVersion) = jitems(1)
          ServerVersionResponse(clientName, protocolVersion)
        case Ping => PingResponse
        case GetAddressHistory(address) =>
          val JArray(jitems) = json.result
          val items = jitems.map(jvalue => {
            val JString(tx_hash) = jvalue \ "tx_hash"
            val height = intField(jvalue, "height")
            TransactionHistoryItem(height, ByteVector32.fromValidHex(tx_hash))
          })
          GetAddressHistoryResponse(address, items)
        case GetScriptHashHistory(scripthash) =>
          val JArray(jitems) = json.result
          val items = jitems.map(jvalue => {
            val JString(tx_hash) = jvalue \ "tx_hash"
            val height = intField(jvalue, "height")
            TransactionHistoryItem(height, ByteVector32.fromValidHex(tx_hash))
          })
          GetScriptHashHistoryResponse(scripthash, items)
        case AddressListUnspent(address) =>
          val JArray(jitems) = json.result
          val items = jitems.map(jvalue => {
            val JString(tx_hash) = jvalue \ "tx_hash"
            val tx_pos = intField(jvalue, "tx_pos")
            val height = intField(jvalue, "height")
            val value = longField(jvalue, "value")
            UnspentItem(ByteVector32.fromValidHex(tx_hash), tx_pos, value, height)
          })
          AddressListUnspentResponse(address, items)
        case ScriptHashListUnspent(scripthash) =>
          val JArray(jitems) = json.result
          val items = jitems.map(jvalue => {
            val JString(tx_hash) = jvalue \ "tx_hash"
            val tx_pos = intField(jvalue, "tx_pos")
            val height = longField(jvalue, "height")
            val value = longField(jvalue, "value")
            UnspentItem(ByteVector32.fromValidHex(tx_hash), tx_pos, value, height)
          })
          ScriptHashListUnspentResponse(scripthash, items)
        case GetTransactionIdFromPosition(height, tx_pos, false) =>
          val JString(tx_hash) = json.result
          GetTransactionIdFromPositionResponse(ByteVector32.fromValidHex(tx_hash), height, tx_pos, Nil)
        case GetTransactionIdFromPosition(height, tx_pos, true) =>
          val JString(tx_hash) = json.result \ "tx_hash"
          val JArray(hashes) = json.result \ "merkle"
          val leaves = hashes collect { case JString(value) => ByteVector32.fromValidHex(value) }
          GetTransactionIdFromPositionResponse(ByteVector32.fromValidHex(tx_hash), height, tx_pos, leaves)
        case _: GetTransaction =>
          val JString(hex) = json.result
          GetTransactionResponse(Transaction read hex)
        case AddressSubscription(address, _) => json.result match {
          case JString(status) => AddressSubscriptionResponse(address, status)
          case _ => AddressSubscriptionResponse(address, "")
        }
        case ScriptHashSubscription(scriptHash, _) => json.result match {
          case JString(status) => ScriptHashSubscriptionResponse(scriptHash, status)
          case _ => ScriptHashSubscriptionResponse(scriptHash, "")
        }
        case BroadcastTransaction(tx) =>
          val JString(message) = json.result
          // if we got here, it means that the server's response does not contain an error and message should be our
          // transaction id. However, it seems that at least on testnet some servers still use an older version of the
          // Electrum protocol and return an error message in the result field
          Try(ByteVector32.fromValidHex(message)) match {
            case Success(txid) if txid == tx.txid => BroadcastTransactionResponse(tx, None)
            case Success(txid) => BroadcastTransactionResponse(tx, Some(Error(1, s"response txid $txid does not match request txid ${tx.txid}")))
            case Failure(_) => BroadcastTransactionResponse(tx, Some(Error(1, message)))
          }
        case GetHeader(height) =>
          val JString(hex) = json.result
          GetHeaderResponse(height, BlockHeader.read(hex))
        case GetHeaders(start_height, _, _) =>
          val max = intField(json.result, "max")
          val JString(hex) = json.result \ "hex"
          val bin = ByteVector.fromValidHex(hex).toArray
          val blockHeaders = bin.grouped(80).map(BlockHeader.read).toList
          GetHeadersResponse(start_height, blockHeaders, max)
        case GetMerkle(txid, _) =>
          val JArray(hashes) = json.result \ "merkle"
          val leaves = hashes collect { case JString(value) => ByteVector32.fromValidHex(value) }
          val blockHeight = intField(json.result, "block_height")
          val JInt(pos) = json.result \ "pos"
          GetMerkleResponse(txid, leaves, blockHeight, pos.toInt)
      }
    }
  }
}
