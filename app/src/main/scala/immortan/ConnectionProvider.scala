package immortan

import java.net.{InetSocketAddress, Socket}
import java.util.concurrent.TimeUnit

import immortan.crypto.Tools
import okhttp3.{OkHttpClient, Request, ResponseBody}


trait ConnectionProvider {
  val proxyAddress: Option[InetSocketAddress]

  val okHttpClient: OkHttpClient

  def getSocket: Socket

  def doWhenReady(action: => Unit): Unit

  def notifyAppAvailable: Unit = Tools.none

  def get(url: String): ResponseBody = {
    val request = (new Request.Builder).url(url).get
    okHttpClient.newCall(request.build).execute.body
  }
}

class ClearnetConnectionProvider extends ConnectionProvider {
  override val okHttpClient: OkHttpClient = (new OkHttpClient.Builder).connectTimeout(15, TimeUnit.SECONDS).build

  override val proxyAddress: Option[InetSocketAddress] = Option.empty

  def doWhenReady(action: => Unit): Unit = action

  override def getSocket: Socket = new Socket
}