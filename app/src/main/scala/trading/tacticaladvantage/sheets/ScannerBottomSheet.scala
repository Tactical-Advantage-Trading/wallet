package trading.tacticaladvantage.sheets

import android.os.Bundle
import android.view.{LayoutInflater, View, ViewGroup}
import android.widget.{ImageButton, TextView}
import androidx.appcompat.view.ContextThemeWrapper
import trading.tacticaladvantage.utils.InputParser
import trading.tacticaladvantage.{BaseActivity, R, WalletApp}
import com.google.android.material.bottomsheet.BottomSheetDialogFragment
import com.journeyapps.barcodescanner.{BarcodeCallback, BarcodeResult, BarcodeView}
import scala.language.implicitConversions
import immortan.Tools._


trait HasBarcodeReader extends BarcodeCallback {
  var lastAttempt: Long = System.currentTimeMillis
  var barcodeReader: BarcodeView = _
  var instruction: TextView = _
  var altAction: TextView = _
}

abstract class ScannerBottomSheet(host: BaseActivity) extends BottomSheetDialogFragment with HasBarcodeReader {
  def resumeBarcodeReader: Unit = runAnd(barcodeReader decodeContinuous this)(barcodeReader.resume)
  def pauseBarcodeReader: Unit = runAnd(barcodeReader setTorch false)(barcodeReader.pause)

  override def onDestroy: Unit = runAnd(barcodeReader.stopDecoding)(super.onStop)
  override def onResume: Unit = runAnd(resumeBarcodeReader)(super.onResume)
  override def onStop: Unit = runAnd(pauseBarcodeReader)(super.onStop)
  var flashlight: ImageButton = _

  override def onCreateView(inflater: LayoutInflater, container: ViewGroup, state: Bundle): View = {
    val contextThemeWrapper = new ContextThemeWrapper(host, R.style.NoActionBar)
    val inflatorExt = inflater.cloneInContext(contextThemeWrapper)
    inflatorExt.inflate(R.layout.sheet_scanner, container, false)
  }

  override def onViewCreated(view: View, savedState: Bundle): Unit = {
    altAction = view.findViewById(R.id.altAction).asInstanceOf[TextView]
    instruction = view.findViewById(R.id.instruction).asInstanceOf[TextView]
    barcodeReader = view.findViewById(R.id.reader).asInstanceOf[BarcodeView]
    flashlight = view.findViewById(R.id.flashlight).asInstanceOf[ImageButton]
    flashlight setOnClickListener host.onButtonTap(toggleTorch)
  }

  def toggleTorch: Unit = {
    val currentTag = flashlight.getTag.asInstanceOf[Int]

    if (currentTag != R.drawable.flashlight_on) {
      flashlight.setImageResource(R.drawable.flashlight_on)
      flashlight.setTag(R.drawable.flashlight_on)
      barcodeReader.setTorch(true)
    } else {
      flashlight.setImageResource(R.drawable.flashlight_off)
      flashlight.setTag(R.drawable.flashlight_off)
      barcodeReader.setTorch(false)
    }
  }
}

class OnceBottomSheet(host: BaseActivity, onCreated: OnceBottomSheet => Unit, onScan: Runnable) extends ScannerBottomSheet(host) {
  def failedScan(error: Throwable): Unit = WalletApp.app.quickToast(error.getMessage)
  def successfulScan(result: Any): Unit = runAnd(dismiss)(onScan.run)

  override def onViewCreated(view: View, savedState: Bundle): Unit = {
    super.onViewCreated(view, savedState)
    onCreated(this)
  }

  override def barcodeResult(scanningResult: BarcodeResult): Unit = for {
    text <- Option(scanningResult.getText) if System.currentTimeMillis - lastAttempt > 2000
    _ = host.runInFutureProcessOnUI(InputParser.recordValue(text), failedScan)(successfulScan)
  } lastAttempt = System.currentTimeMillis
}
