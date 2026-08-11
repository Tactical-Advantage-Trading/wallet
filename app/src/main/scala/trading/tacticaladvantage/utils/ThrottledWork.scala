package trading.tacticaladvantage.utils

import trading.tacticaladvantage.Tools.none


abstract class ThrottledWork[T, V] {
  private var lastWork: Option[T] = None
  private var isWorking = false

  def error(error: Throwable): Unit = none
  def process(data: T, res: V): Unit
  def work(input: T): V

  private def startWork(data: T): Unit = {
    val lifted = for (_ <- Rx.ioQueue) yield work(data)

    lifted.doAfterTerminate {
      val nextWork = synchronized {
        val queued = lastWork
        lastWork = None

        if (queued.isEmpty) {
          isWorking = false
        }

        queued
      }

      nextWork.foreach(startWork)
    }.subscribe(res => process(data, res), error)
  }

  def addWork(data: T): Unit =
    synchronized {
      if (isWorking) {
        lastWork = Some(data)
      } else {
        isWorking = true
        startWork(data)
      }
    }
}
