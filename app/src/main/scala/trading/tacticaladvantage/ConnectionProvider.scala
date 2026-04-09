package trading.tacticaladvantage

import okhttp3.{OkHttpClient, Request, ResponseBody}

import java.net.Socket
import java.util.concurrent.TimeUnit


trait ConnectionProvider {
  val okHttpClient: OkHttpClient

  def getSocket: Socket

  def doWhenReady(action: => Unit): Unit

  def get(url: String): ResponseBody = {
    val request = (new Request.Builder).url(url)
    okHttpClient.newCall(request.get.build).execute.body
  }
}

class ClearnetConnectionProvider extends ConnectionProvider {
  override val okHttpClient: OkHttpClient = (new OkHttpClient.Builder).connectTimeout(15, TimeUnit.SECONDS).build

  def doWhenReady(action: => Unit): Unit = action

  override def getSocket: Socket = new Socket
}