package immortan

import fr.acinq.bitcoin.DeterministicWallet.ExtendedPublicKey

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, ExecutionContextExecutor}
import scala.language.implicitConversions


object Tools {
  type Bytes = Array[Byte]
  type StringList = List[String]
  type Fiat2Btc = Map[String, Double]
  type ExtPubKeys = List[ExtendedPublicKey]
  final val SEPARATOR = " "

  def trimmed(inputText: String): String = inputText.trim.take(144)

  def none: PartialFunction[Any, Unit] = { case _ => }

  def runAnd[T](result: T)(action: Any): T = result

  implicit class Any2Some[T](underlying: T) {
    def asLeft: Left[T, Nothing] = Left(underlying)
    def asRight: Right[Nothing, T] = Right(underlying)
    def asSome: Option[T] = Some(underlying)
  }

  implicit class IterableOfTuple2[T, V](underlying: Iterable[ (T, V) ] = Nil) {
    def secondItems: Iterable[V] = underlying.map { case (_, secondItem) => secondItem }
    def firstItems: Iterable[T] = underlying.map { case (firstItem, _) => firstItem }
  }

  implicit class ThrowableOps(error: Throwable) {
    def stackTraceAsString: String = {
      val stackTraceWriter = new java.io.StringWriter
      error printStackTrace new java.io.PrintWriter(stackTraceWriter)
      stackTraceWriter.toString
    }
  }

  object \ {
    // Useful for matching nested Tuple2 with less noise
    def unapply[A, B](t2: (A, B) /* Got a tuple */) = Some(t2)
  }
}

trait CanBeShutDown {
  def becomeShutDown: Unit
}

abstract class StateMachine[T] { me =>
  def become(freshData: T, freshState: Int): StateMachine[T] = {
    // Update state, data and return itself for easy chaining operations
    state = freshState
    data = freshData
    me
  }

  implicit val context: ExecutionContextExecutor = ExecutionContext.fromExecutor(Executors.newSingleThreadExecutor)
  def !(changeMessage: Any): Unit = scala.concurrent.Future(me !! changeMessage)
  def !!(change: Any): Unit
  var state: Int = -1
  var data: T = _
}