package immortan

import java.net.{InetSocketAddress, Socket}
import java.util.concurrent.TimeUnit
import okhttp3.{MediaType, OkHttpClient, Request, RequestBody, ResponseBody}


trait ConnectionProvider {
  val proxyAddress: Option[InetSocketAddress]

  val okHttpClient: OkHttpClient

  def getSocket: Socket

  def doWhenReady(action: => Unit): Unit

  def get(url: String): ResponseBody = {
    val request = (new Request.Builder).url(url)
    okHttpClient.newCall(request.get.build).execute.body
  }

  def postJson(url: String, json: String): ResponseBody = {
    val body = RequestBody.create(MediaType.parse("application/json; charset=utf-8"), json)
    val request = (new Request.Builder).url(url).addHeader("Content-Type", "application/json")
    okHttpClient.newCall(request.post(body).build).execute.body
  }
}

class ClearnetConnectionProvider extends ConnectionProvider {
  override val okHttpClient: OkHttpClient = (new OkHttpClient.Builder).connectTimeout(15, TimeUnit.SECONDS).build

  override val proxyAddress: Option[InetSocketAddress] = Option.empty

  def doWhenReady(action: => Unit): Unit = action

  override def getSocket: Socket = new Socket
}