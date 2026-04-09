package trading.tacticaladvantage.utils

import trading.tacticaladvantage.Tools.{Bytes, ThrowableOps}
import trading.tacticaladvantage.utils.WsListener._
import com.neovisionaries.ws.client._
import trading.tacticaladvantage.StateMachine
import java.nio.{ByteBuffer, ByteOrder}
import scala.util.Try

object WsListener {
  type JavaList = java.util.List[String]
  type JavaMap = java.util.Map[String, JavaList]

  final val DISCONNECTED = 0
  final val CONNECTED = 1

  case object CmdConnect
  case object CmdConnected
  case object CmdDisconnected
}

class WsListener[T, V](host: StateMachine[T], parse: String => Try[V], errorFun: String => Unit) extends WebSocketAdapter {
  override def onDisconnected(ws: WebSocket, scf: WebSocketFrame, ccf: WebSocketFrame, bySrv: Boolean): Unit = host ! CmdDisconnected
  override def onConnectError(ws: WebSocket, exception: WebSocketException): Unit = host ! CmdDisconnected
  override def onConnected(ws: WebSocket, headers: JavaMap): Unit = host ! CmdConnected

  override def onTextMessage(ws: WebSocket, text: String): Unit =
    parse(text).map(host ! _).recover { case exception =>
      errorFun(exception.stackTraceAsString)
      ws.disconnect
    }
}
