package immortan.sqlite

import java.util.concurrent.atomic.AtomicLong
import rx.lang.scala.Subject

object DbStreams {
  final val updateCounter = new AtomicLong(0)
  final val txStream: Subject[Long] = Subject[Long]
  final val walletStream: Subject[Long] = Subject[Long]

  def next(stream: Subject[Long] = null): Unit =
    stream.onNext(updateCounter.incrementAndGet)
}
