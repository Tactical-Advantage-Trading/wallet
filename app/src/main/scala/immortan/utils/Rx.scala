package immortan.utils

import rx.lang.scala.Observable
import rx.lang.scala.schedulers.IOScheduler
import scala.concurrent.duration._


object Rx {
  def uniqueFirstAndLastWithinWindow[T](obs: Observable[T], window: Duration): Observable[T] =
    obs.throttleFirst(window).merge(obs debounce window).distinctUntilChanged.observeOn(IOScheduler.apply)

  def initDelay[T](next: Observable[T], startMillis: Long, timeoutMillis: Long): Observable[T] = {
    val futureProtectedStartMillis = if (startMillis > System.currentTimeMillis) 0L else startMillis
    val adjustedTimeout = futureProtectedStartMillis + timeoutMillis - System.currentTimeMillis
    val delayLeft = if (adjustedTimeout <= 0L) 0L else adjustedTimeout
    delay(delayLeft).flatMap(_ => next)
  }

  def delay(millis: Long) = Observable.just(null).delay(millis.millis)

  def retry[T](obs: Observable[T], pick: (Throwable, Int) => Duration, times: Range): Observable[T] =
    obs.retryWhen(_.zipWith(Observable from times)(pick) flatMap Observable.timer)

  def repeat[T](obs: Observable[T], pick: (Unit, Int) => Duration, times: Range): Observable[T] =
    obs.repeatWhen(_.zipWith(Observable from times)(pick) flatMap Observable.timer)

  def ioQueue: Observable[Null] = Observable.just(null).subscribeOn(IOScheduler.apply)

  def incSec(errorOrUnit: Any, next: Int): Duration = next.seconds

  def incHour(errorOrUnit: Any, next: Int): Duration = next.hours
}
