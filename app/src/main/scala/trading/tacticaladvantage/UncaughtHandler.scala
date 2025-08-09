package trading.tacticaladvantage

import java.lang.Thread.UncaughtExceptionHandler

import android.app.Activity
import android.content.Intent
import immortan.crypto.Tools.ThrowableOps


object UncaughtHandler {
  val ERROR_REPORT = "errorReport"
}

class UncaughtHandler(ctxt: Activity) extends UncaughtExceptionHandler {
  def uncaughtException(thread: Thread, exception: Throwable): Unit = {
    val emergencyActivity = classOf[EmergencyActivity]
    val intent = new Intent(ctxt, emergencyActivity)

    ctxt startActivity intent.putExtra(UncaughtHandler.ERROR_REPORT, exception.stackTraceAsString)
    android.os.Process killProcess android.os.Process.myPid
    System exit 10
  }
}