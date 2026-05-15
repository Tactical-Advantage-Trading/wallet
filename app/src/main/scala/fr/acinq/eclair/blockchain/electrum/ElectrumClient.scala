package fr.acinq.eclair.blockchain.electrum

import akka.actor.{Actor, ActorRef, Cancellable, Stash, Terminated}
import fr.acinq.bitcoin._
import fr.acinq.eclair.blockchain.bitcoind.rpc.{Error, JsonRPCRequest, JsonRPCResponse}
import fr.acinq.eclair.blockchain.electrum.ElectrumClient._
import io.netty.channel._
import io.netty.channel.nio.NioEventLoopGroup
import io.netty.channel.socket.SocketChannel
import io.netty.channel.socket.nio.NioSocketChannel
import io.netty.handler.codec.string.{LineEncoder, StringDecoder}
import io.netty.handler.codec.{LineBasedFrameDecoder, MessageToMessageDecoder, MessageToMessageEncoder}
import io.netty.handler.ssl.SslContextBuilder
import io.netty.handler.ssl.util.InsecureTrustManagerFactory
import io.netty.util.CharsetUtil
import org.json4s.JsonAST._
import org.json4s.JsonDSL._
import org.json4s.native.JsonMethods
import org.json4s.{JInt, JLong, JString}
import scodec.bits.ByteVector

import java.net.{InetSocketAddress, SocketAddress}
import java.util
import scala.annotation.tailrec
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.util.{Success, Try}


class ElectrumClient(serverAddress: InetSocketAddress, ssl: SSL) extends Actor with Stash {
  val b = new io.netty.bootstrap.Bootstrap
  b channel classOf[NioSocketChannel]
  b group workerGroup

  b.option[java.lang.Boolean](ChannelOption.TCP_NODELAY, true)
  b.option[java.lang.Boolean](ChannelOption.SO_KEEPALIVE, true)
  b.option[java.lang.Integer](ChannelOption.CONNECT_TIMEOUT_MILLIS, 5000)
  b.option(ChannelOption.ALLOCATOR, io.netty.buffer.PooledByteBufAllocator.DEFAULT)

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
      ch.pipeline.addLast(new ExceptionHandler)
    }
  }

  val channelOpenFuture: ChannelFuture = b.connect(serverAddress.getHostName, serverAddress.getPort)

  channelOpenFuture addListeners new ChannelFutureListener {
    override def operationComplete(future: ChannelFuture): Unit =
      if (!future.isSuccess) {
        self ! Close
      } else {
        future.channel.closeFuture addListener new ChannelFutureListener {
          override def operationComplete(future: ChannelFuture): Unit = self ! Close
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
    override def decode(ctx: ChannelHandlerContext, msg: String, out: util.List[AnyRef] = null): Unit =
      out add parseResponse(msg)
  }

  class JsonRPCRequestEncoder extends MessageToMessageEncoder[JsonRPCRequest] {
    override def encode(ctx: ChannelHandlerContext, request: JsonRPCRequest, out: util.List[AnyRef] = null): Unit = {
      val json = ("method" -> request.method) ~ ("params" -> request.params) ~ ("id" -> request.id) ~ ("jsonrpc" -> request.jsonrpc)
      val serialized = JsonMethods.compact(JsonMethods render json)
      out.add(serialized)
    }
  }

  class ActorHandler(actor: ActorRef) extends ChannelInboundHandlerAdapter {
    override def channelActive(ctx: ChannelHandlerContext): Unit = actor ! ctx
    override def channelRead(ctx: ChannelHandlerContext, msg: Any): Unit = actor ! msg
  }

  type ActorSet = Set[ActorRef]
  var scriptHashSubscriptions = Map.empty[ByteVector32, ActorSet]
  val headerSubscriptions = collection.mutable.HashSet.empty[ActorRef]
  val statusListeners = collection.mutable.HashSet.empty[ActorRef]
  val version = ServerVersion(CLIENT_NAME, PROTOCOL_VERSION)
  var reqId = 0

  // We need to regularly send a ping in order not to get disconnected
  val pingTrigger: Cancellable = context.system.scheduler.schedule(30.seconds, 30.seconds, self, Ping)

  override def unhandled(message: Any): Unit =
    message match {
      case Terminated(deadActor) =>
        scriptHashSubscriptions = scriptHashSubscriptions.mapValues(_ - deadActor)
        headerSubscriptions -= deadActor
        statusListeners -= deadActor

      case RemoveStatusListener(actor) =>
        statusListeners -= actor

      case Close =>
        statusListeners.foreach(_ ! ElectrumDisconnected)
        context.stop(self)

      case _ =>
        // Do nothing
    }

  override def postStop: Unit = {
    pingTrigger.cancel
    super.postStop
  }

  def send(ctx: ChannelHandlerContext, request: Request): String = {
    val rpcRequest = request match {
      case GetTransactionIdFromPosition(height, txPos) => JsonRPCRequest(reqId.toString, method = "blockchain.transaction.id_from_pos", params = JInt(height) :: JInt(txPos) :: JBool(true) :: Nil)
      case ServerVersion(clientName, protocolVersion) => JsonRPCRequest(reqId.toString, method = "server.version", params = JString(clientName) :: JString(protocolVersion) :: Nil)
      case ScriptHashSubscription(scriptHash, _) => JsonRPCRequest(reqId.toString, method = "blockchain.scripthash.subscribe", params = JString(scriptHash.toString) :: Nil)
      case GetMerkle(txid, height) => JsonRPCRequest(reqId.toString, method = "blockchain.transaction.get_merkle", params = JString(txid.toString) :: JInt(height) :: Nil)
      case BroadcastTransaction(tx) => JsonRPCRequest(reqId.toString, method = "blockchain.transaction.broadcast", params = JString(Transaction.write(tx).toHex) :: Nil)
      case GetHeaders(start_height, count, _) => JsonRPCRequest(reqId.toString, method = "blockchain.block.headers", params = JInt(start_height) :: JInt(count) :: Nil)
      case ScriptHashListUnspent(scripthash) => JsonRPCRequest(reqId.toString, method = "blockchain.scripthash.listunspent", params = JString(scripthash.toHex) :: Nil)
      case GetScriptHashHistory(scripthash) => JsonRPCRequest(reqId.toString, method = "blockchain.scripthash.get_history", params = JString(scripthash.toHex) :: Nil)
      case GetTransaction(txid) => JsonRPCRequest(reqId.toString, method = "blockchain.transaction.get", params = JString(txid.toString) :: Nil)
      case GetHeader(height) => JsonRPCRequest(reqId.toString, method = "blockchain.block.header", params = JInt(height) :: Nil)
      case _: HeaderSubscription => JsonRPCRequest(reqId.toString, method = "blockchain.headers.subscribe", params = Nil)
      case Ping => JsonRPCRequest(reqId.toString, method = "server.ping", params = Nil)
    }

    if (ctx.channel.isWritable) {
      ctx.channel.writeAndFlush(rpcRequest)
      reqId = reqId + 1
    } else self ! Close
    rpcRequest.id
  }

  def receive: Receive = {
    case ctx: ChannelHandlerContext =>
      context become waitingForVersion(ctx)
      send(ctx, version)

    case AddStatusListener(actor) =>
      statusListeners += actor
  }

  def waitingForVersion(ctx: ChannelHandlerContext): Receive = {
    case Right(json: JsonRPCResponse) =>
      parseJsonResponse(version, json) match {
        case _: ServerVersionResponse =>
          val header = HeaderSubscription(self)
          context become waitingForTip(ctx)
          headerSubscriptions += self
          send(ctx, header)

        case _: ServerError =>
          self ! Close

        case _ =>
        // Do nothing
      }

    case AddStatusListener(actor) =>
      statusListeners += actor
  }

  def waitingForTip(ctx: ChannelHandlerContext): Receive = {
    case Right(json: JsonRPCResponse) =>
      val (height, header) = parseBlockHeader(json.result)
      val ready = ElectrumReady(height, header, serverAddress)
      context become connected(ctx, height, header, Map.empty)
      statusListeners.foreach(_ ! ready)

    case AddStatusListener(actor) =>
      statusListeners += actor
  }

  type ActiveRequest = (Request, ActorRef)
  type ActiveRequests = Map[String, ActiveRequest]

  def connected(ctx: ChannelHandlerContext, height: Int, tip: BlockHeader, requests: ActiveRequests): Receive = {
    case AddStatusListener(actor) =>
      actor ! ElectrumReady(height, tip, serverAddress)
      statusListeners += actor

    case HeaderSubscription(actor) =>
      actor ! HeaderSubscriptionResponse(height, tip)
      headerSubscriptions += actor
      context watch actor

    case request: Request =>
      request match {
        case ScriptHashSubscription(scriptHash, actor) =>
          val sub = scriptHashSubscriptions.getOrElse(scriptHash, Set.empty) + actor
          scriptHashSubscriptions = scriptHashSubscriptions.updated(scriptHash, sub)
          context watch actor

        case _ =>
          // Do nothing
      }

      val active = send(ctx, request) -> (request, sender)
      context become connected(ctx, height, tip, requests + active)

    case Right(json: JsonRPCResponse) =>
      context become connected(ctx, height, tip, requests - json.id)
      requests.get(json.id).collectFirst { case (request, requestor) =>
        requestor ! parseJsonResponse(request, json)
      }

    case Left(response: HeaderSubscriptionResponse) =>
      headerSubscriptions.foreach(_ ! response)

    case Left(response: ScriptHashSubscriptionResponse) =>
      scriptHashSubscriptions.get(response.scriptHash).foreach {
        listeners => listeners.foreach(_ ! response)
      }

    case HeaderSubscriptionResponse(height1, newtip) =>
      context become connected(ctx, height1, newtip, requests)
  }
}

object ElectrumClient {
  val CLIENT_NAME = "3.3.6"
  val PROTOCOL_VERSION = "1.4"
  val workerGroup = new NioEventLoopGroup

  def computeScriptHash(publicKeyScript: ByteVector): ByteVector32 =
    Crypto.sha256(publicKeyScript).reverse

  case class AddStatusListener(actor: ActorRef)
  case class RemoveStatusListener(actor: ActorRef)

  sealed trait Request
  sealed trait Response

  case class ServerVersion(clientName: String, protocolVersion: String) extends Request
  case class ServerVersionResponse(clientName: String, protocolVersion: String) extends Response

  case object Ping extends Request
  case object PingResponse extends Response

  case class TransactionHistoryItem(height: Int, txHash: ByteVector32)

  case class GetScriptHashHistory(scriptHash: ByteVector32) extends Request
  case class GetScriptHashHistoryResponse(scriptHash: ByteVector32, history: List[TransactionHistoryItem] = Nil) extends Response

  case class UnspentItem(txHash: ByteVector32, txPos: Int, value: Long, height: Long) {
    lazy val outPoint = OutPoint(txHash.reverse, txPos)
  }

  case class ScriptHashListUnspent(scriptHash: ByteVector32) extends Request
  case class ScriptHashListUnspentResponse(scriptHash: ByteVector32, unspents: Seq[UnspentItem] = Nil) extends Response

  case class BroadcastTransaction(tx: Transaction) extends Request
  case class BroadcastTransactionResponse(tx: Transaction, error: Option[Error] = None) extends Response

  case class GetTransactionIdFromPosition(height: Int, txPos: Int) extends Request
  case class GetTransactionIdFromPositionResponse(height: Int, txPos: Int, txid: ByteVector32, merkle: List[ByteVector32] = Nil) extends Response {
    lazy val root: ByteVector32 = GetMerkleResponse(txid, merkle, height, txPos).root
  }

  type SideChainNum2BlockHash = (Int, ByteVector32)
  case class GetTransaction(txid: ByteVector32) extends Request
  case class GetTransactionResponse(tx: Transaction) extends Response {
    lazy val sidechainBlockHashes: Seq[SideChainNum2BlockHash] = tx.txOut.map(_.publicKeyScript).collect {
      case bv @ ByteVector(0x6a, 0xd1, 0x61, 0x73, 0x68, sidechainNumber, _*) if bv.length == 6 + 32 =>
        sidechainNumber.toInt -> ByteVector32(bv drop 6)
    }
  }

  case class GetHeader(height: Int) extends Request
  case class GetHeaderResponse(height: Int, header: BlockHeader) extends Response

  case class GetHeaders(startHeight: Int, count: Int, cpHeight: Int = 0) extends Request
  case class GetHeadersResponse(startHeight: Int, headers: Seq[BlockHeader], max: Int) extends Response

  case class GetMerkle(txid: ByteVector32, height: Int) extends Request
  case class GetMerkleResponse(txid: ByteVector32, merkle: List[ByteVector32], blockHeight: Int, pos: Int) extends Response {
    lazy val root: ByteVector32 = loop(txid.reverse +: merkle.map(_.reverse), pos)

    @tailrec
    final def loop(hashes: Seq[ByteVector32], pos: Int): ByteVector32 = if (hashes.length == 1) hashes.head else {
      val hashOrder = if (pos % 2 == 1) hashes(1) ++ hashes.head else hashes.head ++ hashes(1)
      loop(Crypto.hash256(hashOrder) +: hashes.drop(2), pos / 2)
    }
  }

  case class ScriptHashSubscription(scriptHash: ByteVector32, actor: ActorRef) extends Request
  case class ScriptHashSubscriptionResponse(scriptHash: ByteVector32, status: String) extends Response

  case class HeaderSubscription(actor: ActorRef) extends Request
  case class HeaderSubscriptionResponse(height: Int, header: BlockHeader) extends Response

  case class ServerError(request: Request, error: Error) extends Response

  sealed trait ElectrumEvent
  case class ElectrumReady(height: Int, tip: BlockHeader, serverAddress: InetSocketAddress) extends ElectrumEvent
  case object ElectrumDisconnected extends ElectrumEvent

  case object Close
  sealed trait SSL

  object SSL {
    case object DECIDE extends SSL
    case object LOOSE extends SSL
  }

  def parseResponse(input: String): Either[Response, JsonRPCResponse] = {
    val json = JsonMethods.parse(input)

    json \ "method" match {
      case JString(responseMethod) =>
        val JArray(params) = json \ "params"

        Left {
          (responseMethod, params) match {
            case ("blockchain.headers.subscribe", header :: Nil) => HeaderSubscriptionResponse tupled parseBlockHeader(header)
            case ("blockchain.scripthash.subscribe", JString(scriptHashHex) :: JNull :: Nil) => ScriptHashSubscriptionResponse(ByteVector32.fromValidHex(scriptHashHex), "")
            case ("blockchain.scripthash.subscribe", JString(scriptHashHex) :: JString(status) :: Nil) => ScriptHashSubscriptionResponse(ByteVector32.fromValidHex(scriptHashHex), status)
            case _ => throw new RuntimeException
          }
        }

      case _ =>
        val result = parseJsonRpcResponse(json)
        Right(result)
    }
  }

  def parseJsonRpcResponse(json: JValue): JsonRPCResponse = {
    val error = json \ "error" match {
      case JNull | JNothing => None

      case other =>
        val message = other \ "message" match {
          case JString(value) => value
          case _ => ""
        }

        val code = other \ "code" match {
          case JInt(value) => value.intValue
          case JLong(value) => value.intValue
          case _ => 0
        }

        val res = Error(code, message)
        Some(res)
    }

    val id = json \ "id" match {
      case JString(value) => value
      case JInt(value) => value.toString
      case JLong(value) => value.toString
      case _ => ""
    }

    JsonRPCResponse(json \ "result", error, id)
  }

  def longField(jvalue: JValue, field: String): Long =
    (jvalue \ field: @unchecked) match {
      case JLong(value) => value.longValue
      case JInt(value) => value.longValue
    }

  def intField(jvalue: JValue, field: String): Int =
    (jvalue \ field: @unchecked) match {
      case JLong(value) => value.intValue
      case JInt(value) => value.intValue
    }

  def parseBlockHeader(json: JValue): (Int, BlockHeader) = {
    val height = intField(json, "height")
    val JString(hex) = json \ "hex"
    val hdr = BlockHeader.read(hex)
    (height, hdr)
  }

  def parseJsonResponse(request: Request, json: JsonRPCResponse): Response = {
    json.error match {
      case err @ Some(error) => (request: @unchecked) match {
        case BroadcastTransaction(tx) => BroadcastTransactionResponse(tx, err)
        case _ => ServerError(request, error)
      }

      case None => (request: @unchecked) match {
        case Ping => PingResponse

        case _: ServerVersion =>
          val JArray(jitems) = json.result
          val JString(clientName) = jitems.head
          val JString(protocolVersion) = jitems(1)
          ServerVersionResponse(clientName, protocolVersion)

        case GetScriptHashHistory(scripthash) =>
          val JArray(jitems) = json.result
          val items = for (jvalue <- jitems) yield {
            val JString(txHashRaw) = jvalue \ "tx_hash"
            val hash = ByteVector32.fromValidHex(txHashRaw)
            val height = intField(jvalue, "height")
            TransactionHistoryItem(height, hash)
          }

          GetScriptHashHistoryResponse(scripthash, items)

        case ScriptHashListUnspent(scripthash) =>
          val JArray(jitems) = json.result
          val items = for (jvalue <- jitems) yield {
            val JString(txHashRaw) = jvalue \ "tx_hash"
            val hash = ByteVector32.fromValidHex(txHashRaw)
            val tx_pos = intField(jvalue, "tx_pos")
            val height = longField(jvalue, "height")
            val value = longField(jvalue, "value")
            UnspentItem(hash, tx_pos, value, height)
          }

          ScriptHashListUnspentResponse(scripthash, items)

        case _: GetTransaction =>
          val JString(hex) = json.result
          GetTransactionResponse(Transaction read hex)

        case ScriptHashSubscription(scriptHash, _) => json.result match {
          case JString(status) => ScriptHashSubscriptionResponse(scriptHash, status)
          case _ => ScriptHashSubscriptionResponse(scriptHash, "")
        }

        case BroadcastTransaction(tx) =>
          val JString(message) = json.result
          Try(ByteVector32 fromValidHex message) match {
            case Success(txid) if txid == tx.txid => BroadcastTransactionResponse(tx, None)
            case _ => BroadcastTransactionResponse(tx, Some(Error(1, message)))
          }

        case GetHeader(height) =>
          val JString(hex) = json.result
          val hdr = BlockHeader.read(hex)
          GetHeaderResponse(height, hdr)

        case GetHeaders(start_height, _, _) =>
          val max = intField(json.result, "max")
          val JString(hex) = json.result \ "hex"
          val bin = ByteVector.fromValidHex(hex).toArray
          val blockHeaders = bin.grouped(80).map(BlockHeader.read)
          GetHeadersResponse(start_height, blockHeaders.toList, max)

        case GetMerkle(txid, _) =>
          val JInt(pos) = json.result \ "pos"
          val JArray(hashes) = json.result \ "merkle"
          val blockHeight = intField(json.result, "block_height")
          val leaves = hashes collect { case JString(stringValue) => stringValue }
          GetMerkleResponse(txid, leaves.map(ByteVector32.fromValidHex), blockHeight, pos.toInt)

        case GetTransactionIdFromPosition(height, txPos) =>
          val JString(txid) = json.result \ "tx_hash"
          val JArray(hashes) = json.result \ "merkle"

          val merkle = hashes.collect { case JString(hash) => ByteVector32 fromValidHex hash }
          GetTransactionIdFromPositionResponse(height, txPos, ByteVector32 fromValidHex txid, merkle)
      }
    }
  }
}
