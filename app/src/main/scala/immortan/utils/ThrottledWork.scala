package immortan.utils

import immortan.crypto.Tools.{Any2Some, none}
import rx.lang.scala.Subscription


abstract class ThrottledWork[T, V] {
  private var lastWork: Option[T] = None
  private var subscription: Option[Subscription] = None

  def error(error: Throwable): Unit = none
  def process(data: T, res: V): Unit
  def work(input: T): V

  private def runOnError(err: Throwable): Unit = {
    // First nullify sunscription, then process error
    subscription = None
    error(err)
  }

  private def runOnNext(data: T, res: V): Unit = {
    // First nullify sunscription, then process callback
    subscription = None
    process(data, res)
  }

  def addWork(data: T): Unit =
    if (subscription.isEmpty) {
      val lifted = for (_ <- Rx.ioQueue) yield work(data)
      // Previous work has already been finished by now or has never started at all
      // schedule a new one and then look if more work is added once this one is done

      subscription = lifted
        .doOnSubscribe { lastWork = None }
        .doAfterTerminate(lastWork foreach addWork)
        .subscribe(res => runOnNext(data, res), runOnError)
        .asSome

    } else {
      // Current work has not finished yet
      // schedule new work once this is done
      lastWork = Some(data)
    }

  def replaceWork(data: T): Unit =
    if (subscription.isEmpty) {
      val lifted = for (_ <- Rx.ioQueue) yield work(data)
      // Previous work has already finished or was interrupted or has never been started
      subscription = lifted.subscribe(res => process(data, res), error).asSome
    } else {
      // Current work has not finished yet
      // disconnect subscription and replace
      unsubscribeCurrentWork
      replaceWork(data)
    }

  def unsubscribeCurrentWork: Unit = {
    subscription.foreach(_.unsubscribe)
    subscription = None
  }
}