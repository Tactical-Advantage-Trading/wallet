package trading.tacticaladvantage.utils

import com.neovisionaries.ws.client._
import immortan.Tools.ThrowableOps
import scala.util.Try
import WsListener._
import immortan.StateMachine

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

  override def onTextMessage(ws: WebSocket, message: String): Unit =
    parse(message).map(host ! _).recover { case exception =>
      errorFun(exception.stackTraceAsString)
      ws.disconnect
    }
}
