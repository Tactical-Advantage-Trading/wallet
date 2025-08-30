package trading.tacticaladvantage

import android.content.pm.PackageManager
import android.content.{DialogInterface, Intent}
import android.graphics.Bitmap.Config.ARGB_8888
import android.graphics.{Bitmap, Color}
import android.net.Uri
import android.os.Bundle
import android.text.method.LinkMovementMethod
import android.text.{Editable, Spanned, TextWatcher}
import android.view.View.OnClickListener
import android.view.{View, ViewGroup, WindowManager}
import android.widget._
import androidx.appcompat.app.{AlertDialog, AppCompatActivity}
import androidx.appcompat.widget.AppCompatButton
import androidx.cardview.widget.CardView
import androidx.core.app.ActivityCompat
import androidx.core.content.{ContextCompat, FileProvider}
import androidx.recyclerview.widget.RecyclerView
import com.cottacush.android.currencyedittext.CurrencyEditText
import com.google.android.material.slider.Slider
import com.google.android.material.textfield.TextInputLayout
import com.google.zxing.qrcode.QRCodeWriter
import com.google.zxing.qrcode.decoder.ErrorCorrectionLevel
import com.google.zxing.{BarcodeFormat, EncodeHintType}
import com.ornach.nobobutton.NoboButton
import fr.acinq.bitcoin.DeterministicWallet.ExtendedPublicKey
import fr.acinq.bitcoin._
import fr.acinq.eclair._
import fr.acinq.eclair.blockchain.electrum._
import fr.acinq.eclair.blockchain.fee.{FeeratePerByte, FeeratePerKw}
import immortan.Tools._
import immortan.sqlite.CompleteUsdtWalletInfo
import immortan.utils._
import org.apmem.tools.layouts.FlowLayout
import trading.tacticaladvantage.BaseActivity.StringOps
import trading.tacticaladvantage.Colors._
import trading.tacticaladvantage.R.string._
import trading.tacticaladvantage.utils.{BitcoinUri, InputParser}

import java.io.{File, FileOutputStream}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.language.implicitConversions
import scala.util.{Failure, Success}


object BaseActivity {
  implicit class StringOps(source: String) {
    def html: Spanned = android.text.Html.fromHtml(source)
    def humanFour: String = "<tt>" + source.grouped(4).mkString(s"\u0020") + "</tt>"
    def short0x: String = s"<tt><i>${source take 2}</i></tt>&#160;${source.drop(2).short}"

    def short: String = {
      val len = source.length
      val firstFirst = source.slice(0, 4)
      val secondFirst = source.slice(4, 8)
      val secondLast = source.slice(len - 4, len)
      val doubleSmall = "<sup><small><small>&#8230;</small></small></sup>"
      s"<tt>$firstFirst&#160;$secondFirst&#160;$doubleSmall&#160;$secondLast</tt>"
    }
  }
}

object ClassNames {
  val qrUsdtActivityClass: Class[QRUsdtActivity] = classOf[QRUsdtActivity]
  val qrBtcActivityClass: Class[QRBtcActivity] = classOf[QRBtcActivity]
  val qrSigActivityClass: Class[QRSigActivity] = classOf[QRSigActivity]
  val mainActivityClass: Class[MainActivity] = classOf[MainActivity]
  val taActivityClass: Class[TaActivity] = classOf[TaActivity]
}

object Colors {
  val cardIn: String = "#" + WalletApp.app.getResources.getString(R.color.colorAccent).substring(3)
  val cardOut: String = "#" + WalletApp.app.getResources.getString(R.color.cardOutText).substring(3)
  val cardZero: String = "#" + WalletApp.app.getResources.getString(R.color.cardZeroText).substring(3)
  val signCardZero: String = "#" + WalletApp.app.getResources.getString(R.color.signCardZeroText).substring(3)
}

trait ExternalDataChecker {
  def checkExternalData(onNothing: Runnable): Unit
  val noneRunnable: Runnable = new Runnable {
    def run: Unit = none
  }
}

trait ChoiceReceiver {
  def onChoiceMade(tag: AnyRef, pos: Int): Unit
}

trait BaseActivity extends AppCompatActivity { me =>
  lazy val qrSize: Int = getResources.getDimensionPixelSize(R.dimen.qr_size)
  val nothingUsefulTask: Runnable = UITask(WalletApp.app quickToast error_nothing_useful)
  val timer: java.util.Timer = new java.util.Timer

  val exitTo: Class[_] => Any = target => {
    this startActivity new Intent(me, target)
    finish
  }

  val goTo: Class[_] => Any = target => {
    this startActivity new Intent(me, target)
    InputParser.DoNotEraseRecordedValue
  }

  def goToWithValue(target: Class[_], value: Any): Any = {
    // Utility method in case if target expects a value
    InputParser.value = value
    goTo(target)
  }

  def START(state: Bundle): Unit

  override def onCreate(savedActivityState: Bundle): Unit = {
    Thread setDefaultUncaughtExceptionHandler new UncaughtHandler(me)
    super.onCreate(savedActivityState)
    START(savedActivityState)
  }

  override def onDestroy: Unit = {
    super.onDestroy
    timer.cancel
  }

  // Helpers

  def titleViewFromUri(uri: BitcoinUri): TitleView = {
    val label = uri.label.map(label => s"<br><br><b>$label</b>").getOrElse(new String)
    val message = uri.message.map(message => s"<br><i>$message<i>").getOrElse(new String)
    val caption = getString(dialog_send_btc).format(uri.address.short, label + message)
    new TitleView(caption)
  }

  def browse(maybeUri: String): Unit = try {
    me startActivity new Intent(Intent.ACTION_VIEW, Uri parse maybeUri)
  } catch { case exception: Throwable => me onFail exception }

  def share(text: CharSequence): Unit = startActivity {
    val shareAction = (new Intent).setAction(Intent.ACTION_SEND)
    shareAction.setType("text/plain").putExtra(Intent.EXTRA_TEXT, text)
  }

  // Listener helpers

  def onButtonTap(fun: => Unit): OnClickListener = new OnClickListener {
    def onClick(view: View): Unit = fun
  }

  def onTextChange(fun: String => Unit): TextWatcher = new TextWatcher {
    override def onTextChanged(c: CharSequence, x: Int, y: Int, z: Int): Unit = fun(c.toString)
    override def beforeTextChanged(s: CharSequence, x: Int, y: Int, z: Int): Unit = none
    override def afterTextChanged(e: Editable): Unit = none
  }

  def runInFutureProcessOnUI[T](fun: => T, no: Throwable => Unit)(ok: T => Unit): Unit = runFutureProcessOnUI[T](Future(fun), no)(ok)

  def runFutureProcessOnUI[T](fun: Future[T], no: Throwable => Unit)(ok: T => Unit): Unit = fun onComplete {
    case Success(result) => UITask(ok apply result).run case Failure(error) => UITask(no apply error).run
  }

  def setVis(isVisible: Boolean, view: View): Unit = {
    val nextMode = if (isVisible) View.VISIBLE else View.GONE
    if (view.getVisibility != nextMode) view.setVisibility(nextMode)
  }

  def setVisMany(items: (Boolean, View)*): Unit = {
    for (isVisible \ view <- items) setVis(isVisible, view)
  }

  def UITask(fun: => Any): java.util.TimerTask = {
    val runnableExec = new Runnable { override def run: Unit = fun }
    new java.util.TimerTask { def run: Unit = me runOnUiThread runnableExec }
  }

  // Builders

  def clickableTextField(view: View): TextView = {
    val field: TextView = view.asInstanceOf[TextView]
    field setMovementMethod LinkMovementMethod.getInstance
    field
  }

  def titleBodyAsViewBuilder(title: View, body: View): AlertDialog.Builder =
    new AlertDialog.Builder(me).setCustomTitle(title).setView(body)

  def onFail(error: String): Unit = UITask {
    val bld = titleBodyAsViewBuilder(null, error.asDefView)
    val bld1 = bld.setPositiveButton(dialog_ok, null)
    showForm(bld1.create)
  }.run

  def onFail(error: Throwable): Unit = error match {
    case exc if exc.getCause.isInstanceOf[java.io.InterruptedIOException] =>
    case _ => onFail(error.toString)
  }

  def getPositiveButton(alert: AlertDialog): Button = alert.getButton(DialogInterface.BUTTON_POSITIVE)

  def getNegativeButton(alert: AlertDialog): Button = alert.getButton(DialogInterface.BUTTON_NEGATIVE)

  def getNeutralButton(alert: AlertDialog): Button = alert.getButton(DialogInterface.BUTTON_NEUTRAL)

  def mkCheckForm(ok: AlertDialog => Unit, no: => Unit, bld: AlertDialog.Builder, okRes: Int, noRes: Int): AlertDialog = {
    // Create alert dialog where NEGATIVE button removes a dialog AND calls a respected provided function
    // both POSITIVE and NEGATIVE buttons may be omitted by providing -1 as their resource ids
    if (-1 != noRes) bld.setNegativeButton(noRes, null)
    if (-1 != okRes) bld.setPositiveButton(okRes, null)
    val alert = showForm(bld.create)

    val posAct = onButtonTap {
      ok(alert)
    }

    val negAct = onButtonTap {
      alert.dismiss
      no
    }

    if (-1 != noRes) getNegativeButton(alert) setOnClickListener negAct
    if (-1 != okRes) getPositiveButton(alert) setOnClickListener posAct
    alert
  }

  def mkCheckFormNeutral(ok: AlertDialog => Unit, no: => Unit, neutral: AlertDialog => Unit, bld: AlertDialog.Builder, okRes: Int, noRes: Int, neutralRes: Int): AlertDialog = {

    if (-1 != neutralRes) bld.setNeutralButton(neutralRes, null)
    val alert = mkCheckForm(ok, no, bld, okRes, noRes)

    val neutralAct = onButtonTap {
      neutral(alert)
    }

    // Extend base dialog with a special NEUTRAL button, may be omitted by providing -1
    if (-1 != neutralRes) getNeutralButton(alert) setOnClickListener neutralAct
    alert
  }

  def showForm(alertDialog: AlertDialog): AlertDialog = {
    // Display dialog taking into account that host activity may not be there
    // Popup window is not cancellable on touch outside and may be width-limited

    try {
      alertDialog.show
      // First, limit popup window width in case if this is a tablet, then make sure it does not blow up if called on destroyed activity
      if (WalletApp.app.scrWidth > 2.3) alertDialog.getWindow.setLayout(WalletApp.app.maxDialog.toInt, ViewGroup.LayoutParams.WRAP_CONTENT)
      clickableTextField(alertDialog findViewById android.R.id.message)
      alertDialog.setCanceledOnTouchOutside(false)
    } catch none
    alertDialog
  }

  // Scanner

  final val scannerRequestCode = 101

  def callScanner(sheet: sheets.ScannerBottomSheet): Unit = {
    val allowed = ContextCompat.checkSelfPermission(me, android.Manifest.permission.CAMERA) == PackageManager.PERMISSION_GRANTED
    if (!allowed) ActivityCompat.requestPermissions(me, Array(android.Manifest.permission.CAMERA), scannerRequestCode)
    else sheet.show(getSupportFragmentManager, "scanner-bottom-sheet-fragment")
  }

  def addFlowChip(flow: FlowLayout, chipText: String, backgroundRes: Int, shareText: Option[String] = None): TextView =
    addFlowChip(flow, chipText, backgroundRes)(shareText foreach WalletApp.app.copy)

  def addFlowChip(flow: FlowLayout, chipText: String, backgroundRes: Int)(onTap: => Unit): TextView = {
    val text = getLayoutInflater.inflate(R.layout.frag_chip_text, flow, false).asInstanceOf[TextView]
    text setOnClickListener onButtonTap(onTap)
    text setBackgroundResource backgroundRes
    text setText chipText.html

    flow setVisibility View.VISIBLE
    flow addView text
    text
  }

  def singleInputPopup: (View, TextInputLayout, EditText, CheckBox, TextView) = {
    val container = getLayoutInflater.inflate(R.layout.frag_hint_input, null, false)
    val extraOption = container.findViewById(R.id.extraOption).asInstanceOf[CheckBox]
    val extraOptionText = container.findViewById(R.id.extraOptionText).asInstanceOf[TextView]
    val extraInputLayout = container.findViewById(R.id.extraInputLayout).asInstanceOf[TextInputLayout]
    val extraInput = container.findViewById(R.id.extraInput).asInstanceOf[EditText]
    (container, extraInputLayout, extraInput, extraOption, extraOptionText)
  }

  def singleInputPopupBuilder = {
    val (container, extraInputLayout, extraInput, _, _) = singleInputPopup
    (titleBodyAsViewBuilder(null, container), extraInputLayout, extraInput)
  }

  // Rich popup title

  implicit class TitleView(titleText: String) {
    val view: LinearLayout = getLayoutInflater.inflate(R.layout.frag_top_tip, null).asInstanceOf[LinearLayout]
    val flow: FlowLayout = view.findViewById(R.id.tipExtraTags).asInstanceOf[FlowLayout]
    val tipTitle: TextView = clickableTextField(view findViewById R.id.tipTitle)
    tipTitle.setText(titleText.html)

    def asDefView: LinearLayout = {
      view.setBackgroundColor(0x22AAAAAA)
      view
    }

    def asColoredView(colorRes: Int): LinearLayout = {
      val resultColor = ContextCompat.getColor(me, colorRes)
      view.setBackgroundColor(resultColor)
      view
    }
  }

  // Fiat / BTC converter

  def updatePopupButton(button: Button, isEnabled: Boolean): Unit = {
    val alpha = if (isEnabled) 1F else 0.3F
    button.setEnabled(isEnabled)
    button.setAlpha(alpha)
  }

  class RateManagerContent(val container: View) {
    def updateFiatText(value: String): Unit = runAnd(fiatInputAmount.requestFocus)(fiatInputAmount setText value)
    def updateBtcText(value: String): Unit = runAnd(inputAmount.requestFocus)(inputAmount setText value)
    val fiatInputAmount = container.findViewById(R.id.fiatInputAmount).asInstanceOf[CurrencyEditText]
    val fiatInputAmountHint = container.findViewById(R.id.fiatInputAmountHint).asInstanceOf[TextView]
    val inputAmount = container.findViewById(R.id.inputAmount).asInstanceOf[CurrencyEditText]
    val inputAmountHint = container.findViewById(R.id.inputAmountHint).asInstanceOf[TextView]
    val hintFiatDenom = clickableTextField(container findViewById R.id.hintFiatDenom)
    val hintDenom = clickableTextField(container findViewById R.id.hintDenom)
  }

  class RateManager(val rmc: RateManagerContent, fiatCode: String) {
    val rates = WalletApp.fiatRates.info.rates

    def updateText(value: MilliSatoshi): Unit = {
      rmc.updateBtcText(BtcDenom.fromMsat(value).toString)
      updateFiatInput
    }

    def bigDecimalFrom(input: CurrencyEditText): BigDecimal = BigDecimal(input.getNumericValueBigDecimal)
    def resultMsat: MilliSatoshi = (bigDecimalFrom(rmc.inputAmount) * BtcDenom.factor).toLong.msat

    def updatedFiatFromBtc: String =
      WalletApp.msatInFiat(rates, fiatCode)(resultMsat)
        .filter(0.001D <= _).map(_.toString)
        .getOrElse("0.00")

    def updatedBtcFromFiat: String =
      WalletApp.currentRate(rates, fiatCode)
        .map(perBtc => bigDecimalFrom(rmc.fiatInputAmount) / perBtc)
        .filter(0.000000001D <= _).map(Denomination.btcBigDecimal2MSat)
        .map(BtcDenom.fromMsat).map(_.toString)
        .getOrElse("0.00")

    def updateFiatInput: Unit = {
      rmc.fiatInputAmount setText updatedFiatFromBtc
      rmc.fiatInputAmount setMaxNumberOfDecimalDigits 2
    }

    def updateBtcInput: Unit = {
      rmc.inputAmount setText updatedBtcFromFiat
      rmc.inputAmount setMaxNumberOfDecimalDigits 8
    }

    rmc.fiatInputAmount addTextChangedListener onTextChange { _ =>
      if (rmc.fiatInputAmount.hasFocus) updateBtcInput
    }

    rmc.inputAmount addTextChangedListener onTextChange { _ =>
      if (rmc.inputAmount.hasFocus) updateFiatInput
    }

    rmc.inputAmountHint setText BtcDenom.sign.toUpperCase
    rmc.fiatInputAmountHint setText fiatCode.toUpperCase
    rmc.fiatInputAmount setLocale Denomination.locale
    rmc.inputAmount setLocale Denomination.locale
  }

  class TwoSidedItem(val parent: View, firstText: CharSequence, secondText: CharSequence) {
    val secondItem = parent.findViewById(R.id.secondItem).asInstanceOf[TextView]
    val firstItem = parent.findViewById(R.id.firstItem).asInstanceOf[TextView]
    secondItem.setText(secondText)
    firstItem.setText(firstText)
  }

  class FeeViewContent(val container: View) {
    val feeRate = container.findViewById(R.id.feeRate).asInstanceOf[TextView]
    val bitcoinFee = container.findViewById(R.id.bitcoinFee).asInstanceOf[TextView]
    val fiatFee = container.findViewById(R.id.fiatFee).asInstanceOf[TextView]

    val customFeerate = container.findViewById(R.id.customFeerate).asInstanceOf[Slider]
    val customFeerateOption = container.findViewById(R.id.customFeerateOption).asInstanceOf[TextView]
  }

  class FeeView[T](from: FeeratePerByte, val fvc: FeeViewContent) {
    var worker: ThrottledWork[String, T] = _
    var rate: FeeratePerKw = _

    def update(feeOpt: Option[MilliSatoshi], showIssue: Boolean): Unit = {
      val satPerVirtualByteFee = getString(dialog_fee).format(FeeratePerByte(rate).feerate.toLong)
      setVisMany(feeOpt.isDefined -> fvc.bitcoinFee, feeOpt.isDefined -> fvc.fiatFee)
      fvc.feeRate.setText(s"$satPerVirtualByteFee sat/vB".html)

      feeOpt.foreach { fee =>
        fvc.fiatFee setText WalletApp.currentMsatInFiatHuman(fee).html
        fvc.bitcoinFee setText BtcDenom.parsed(fee, cardIn, cardZero).html
      }
    }

    private val revealSlider = onButtonTap {
      val currentFeerate = FeeratePerByte(rate).feerate.toLong
      setVisMany(false -> fvc.customFeerateOption, true -> fvc.customFeerate)
      fvc.customFeerate.setValueFrom(from.feerate.toLong)
      fvc.customFeerate.setValueTo(currentFeerate * 10)
      fvc.customFeerate.setValue(currentFeerate)
    }

    setVis(isVisible = true, fvc.customFeerateOption)
    fvc.customFeerateOption.setOnClickListener(revealSlider)
    fvc.feeRate.setOnClickListener(revealSlider)

    fvc.customFeerate addOnChangeListener new Slider.OnChangeListener {
      override def onValueChange(slider: Slider, value: Float, fromUser: Boolean): Unit = {
        val feeratePerByte = FeeratePerByte(value.toLong.sat)
        rate = FeeratePerKw(feeratePerByte)
        worker addWork "SLIDER-CHANGE"
      }
    }
  }

  // Chan TX popup for wallets

  sealed trait HasHostView {
    val host: View
  }

  class EditView(val host: View) extends HasHostView {
    val rmc = new RateManagerContent(host)
    val fvc = new FeeViewContent(host)
  }

  class CPFPView(val host: View) extends HasHostView {
    val cpfpCurrent = new TwoSidedItem(host.findViewById(R.id.cpfpCurrent), getString(tx_cpfp_current), new String)
    val cpfpAfter = new TwoSidedItem(host.findViewById(R.id.cpfpAfter), getString(tx_cpfp_rbf_after), new String)
    val fvc = new FeeViewContent(host)
  }

  class RBFView(val host: View) extends HasHostView {
    val rbfCurrent = new TwoSidedItem(host.findViewById(R.id.rbfCurrent), getString(tx_rbf_current), new String)
    val rbfIssue = host.findViewById(R.id.rbfIssue).asInstanceOf[TextView]
    val fvc = new FeeViewContent(host)
  }

  class ConfirmView(val host: View) extends HasHostView {
    val chainNextButton = host.findViewById(R.id.chainNextButton).asInstanceOf[NoboButton]
    val chainEditButton = host.findViewById(R.id.chainEditButton).asInstanceOf[NoboButton]
    val chainCancelButton = host.findViewById(R.id.chainCancelButton).asInstanceOf[NoboButton]
    val confirmFiat = new TwoSidedItem(host.findViewById(R.id.confirmFiat), getString(dialog_send_btc_confirm_fiat), new String)
    val confirmAmount = new TwoSidedItem(host.findViewById(R.id.confirmAmount), getString(dialog_send_confirm_amount), new String)
    val confirmFee = new TwoSidedItem(host.findViewById(R.id.confirmFee), getString(dialog_send_confirm_fee), new String)
  }

  sealed trait SendView {
    val body = getLayoutInflater.inflate(R.layout.frag_input_chain, null)
    val editChain = body.findViewById(R.id.editChain).asInstanceOf[LinearLayout]
    val inputChain = body.findViewById(R.id.inputChain).asInstanceOf[LinearLayout]

    val editView = new EditView(editChain)
    val rbfView = new RBFView(body findViewById R.id.rbf)
    val cpfpView = new CPFPView(body findViewById R.id.cpfp)
    val confirmView = new ConfirmView(body findViewById R.id.confirmChain)
    val views = List(editView, rbfView, cpfpView, confirmView)
    var defaultView: HasHostView = editView

    def switchTo(visibleSection: HasHostView): Unit = for (candidateSection <- views) setVis(isVisible = candidateSection == visibleSection, candidateSection.host)
    def switchButtons(alert: AlertDialog, on: Boolean): Unit = setVisMany(on -> getPositiveButton(alert), on -> getNegativeButton(alert), on -> getNeutralButton(alert), true -> body)

    def switchToDefault(alert: AlertDialog): Unit = {
      switchButtons(alert, on = true)
      switchTo(defaultView)
    }
  }

  class BtcSendView(val specs: Seq[WalletSpec] = Nil) extends SendView { self =>
    val totalCanSend = specs.map(_.info.lastBalance).sum.toMilliSatoshi
    val canSendFiat = WalletApp.currentMsatInFiatHuman(totalCanSend)
    val canSend = BtcDenom.parsedTT(totalCanSend, cardIn, cardZero)
    val rm = new RateManager(editView.rmc, WalletApp.fiatCode)

    editView.rmc.hintFiatDenom setText getString(dialog_up_to).format(canSendFiat).html
    editView.rmc.hintDenom setText getString(dialog_up_to).format(canSend).html
  }

  class UsdtSendView(val info: CompleteUsdtWalletInfo) extends SendView { self =>
    setVisMany(false -> confirmView.confirmFiat.parent, false -> editView.fvc.customFeerateOption)
    setVisMany(false -> editView.rmc.inputAmount, false -> editView.rmc.inputAmountHint, false -> editView.rmc.hintDenom)
    editView.rmc.hintFiatDenom setText getString(dialog_up_to).format(Denomination.fiatTT("0", info.lastBalance, null, cardIn, isIncoming = false).trim).html
    editView.fvc.feeRate setText getString(dialog_fee).format(s"<font color=$cardZero>${me getString dialog_fee_estimating}</font>").html
    editView.rmc.fiatInputAmountHint setText usdt_wallet
  }

  abstract class BtcWalletSelector(title: TitleView) {
    val info = addFlowChip(title.flow, getString(select_wallets), R.drawable.border_yellow, None)
    val cardsContainer = getLayoutInflater.inflate(R.layout.frag_linear_layout, null).asInstanceOf[LinearLayout]
    val alert = mkCheckForm(alert => runAnd(alert.dismiss)(onOk), none, titleBodyAsViewBuilder(title.view, cardsContainer), dialog_ok, dialog_cancel)
    val chooser = new WalletCardManager(cardsContainer)

    lazy val cards: Iterable[BtcWalletCard] = for {
      xPub \ spec <- ElectrumWallet.specs if spec.spendable
    } yield new BtcWalletCard(me, xPub) {
      setVis(isVisible = false, view = infoWalletNotice)
      def onWalletTap: Unit = runAnd { isSelected = !isSelected } {
        updatePopupButton(getPositiveButton(alert), chosenCards.nonEmpty)
        val totalCanSend = chosenCards.map(_.info.lastBalance).sum.toMilliSatoshi
        val formatted = BtcDenom.parsedTT(totalCanSend, cardIn, cardZero)
        if (totalCanSend > 0L.msat) info.setText(s"∑ $formatted".html)
        else info.setText(select_wallets)
        updateView
      }
    }

    def onOk: Unit
    def chosenCards: Iterable[WalletSpec] = cards.filter(_.isSelected).flatMap(ElectrumWallet.specs get _.xPub)
    updatePopupButton(button = getPositiveButton(alert), isEnabled = false)
    runAnd(chooser)(chooser init cards.toList).unPad
    chooser.cardViews.foreach(_.updateView)
  }
}

trait BaseCheckActivity extends BaseActivity { me =>
  def PROCEED(state: Bundle): Unit

  override def START(state: Bundle): Unit = {
    if (WalletApp.isAlive && WalletApp.isOperational) PROCEED(state) else {
      // The way Android works is we can get some objects nullified when restoring from background
      // when that happens we make sure to free all remaining resources and start from scratch
      WalletApp.freePossiblyUsedRuntimeResouces
      exitTo(ClassNames.mainActivityClass)
    }
  }
}

trait QRActivity extends BaseCheckActivity { me =>
  def shareData(bitmap: Bitmap, text: String): Unit = {
    val paymentRequestFilePath = new File(getCacheDir, "images")
    if (!paymentRequestFilePath.isFile) paymentRequestFilePath.mkdirs
    val out = new FileOutputStream(s"$paymentRequestFilePath/qr.png")
    bitmap.compress(Bitmap.CompressFormat.PNG, 85, out)
    out.close

    val savedFile = new File(paymentRequestFilePath, "qr.png")
    val fileURI = FileProvider.getUriForFile(me, "trading.tacticaladvantage", savedFile)
    val share = new Intent setAction Intent.ACTION_SEND setType "text/plain" addFlags Intent.FLAG_GRANT_READ_URI_PERMISSION
    share.putExtra(Intent.EXTRA_TEXT, text).putExtra(Intent.EXTRA_STREAM, fileURI).setDataAndType(fileURI, getContentResolver getType fileURI)
    me startActivity Intent.createChooser(share, "Choose an app")
  }

  class QRViewHolder(itemView: View) extends RecyclerView.ViewHolder(itemView) {
    val qrCode: ImageView = itemView.findViewById(R.id.qrCode).asInstanceOf[ImageView]
    val qrLabel: TextView = itemView.findViewById(R.id.qrLabel).asInstanceOf[TextView]
    val qrShare: AppCompatButton = itemView.findViewById(R.id.qrShare).asInstanceOf[AppCompatButton]
    val qrEdit: AppCompatButton = itemView.findViewById(R.id.qrEdit).asInstanceOf[AppCompatButton]
    val qrCopy: AppCompatButton = itemView.findViewById(R.id.qrCopy).asInstanceOf[AppCompatButton]
  }
}

object QRActivity {
  val writer = new QRCodeWriter
  val hints = new java.util.Hashtable[EncodeHintType, Any]
  hints.put(EncodeHintType.ERROR_CORRECTION, ErrorCorrectionLevel.M)
  hints.put(EncodeHintType.MARGIN, 1)

  def get(data: String, size: Int): Bitmap = {
    val bitMatrix = writer.encode(data, BarcodeFormat.QR_CODE, size, size, hints)
    val (width, height) = (bitMatrix.getWidth, bitMatrix.getHeight)
    val pixels = new Array[Int](width * height)

    for {
      xPos <- 0 until width
      yPos <- 0 until height
      isBlack = bitMatrix.get(xPos, yPos)
      color = if (isBlack) Color.BLACK else Color.WHITE
    } pixels(yPos * width + xPos) = color

    val qrBitmap = Bitmap.createBitmap(width, height, ARGB_8888)
    qrBitmap.setPixels(pixels, 0, size, 0, 0, width, height)
    qrBitmap
  }
}

// WALLET CARDS

class WalletCardManager(holder: LinearLayout) {
  var cardViews = List.empty[WalletCard]

  def init(cards: List[WalletCard] = Nil): Unit = {
    cards.foreach(holder addView _.cardWrap)
    cardViews = cards
  }

  def unPad: Unit = cardViews.foreach { card =>
    val padding: Int = card.cardWrap.getPaddingTop
    card.cardWrap.setPadding(padding, padding, padding, 0)
  }
}

abstract class WalletCard(host: BaseActivity) {
  val cardWrap: LinearLayout = host.getLayoutInflater.inflate(R.layout.frag_wallet_card, null).asInstanceOf[LinearLayout]
  val imageTip: ImageView = cardWrap.findViewById(R.id.imageTip).asInstanceOf[ImageView]
  val cardView: CardView = cardWrap.findViewById(R.id.cardView).asInstanceOf[CardView]
  cardView setOnClickListener host.onButtonTap(onWalletTap)

  val infoContainer: View = cardWrap.findViewById(R.id.infoContainer).asInstanceOf[View]
  val infoWalletLabel: TextView = cardWrap.findViewById(R.id.infoWalletLabel).asInstanceOf[TextView]
  val infoWalletNotice: TextView = cardWrap.findViewById(R.id.infoWalletNotice).asInstanceOf[TextView]
  infoWalletNotice setText tap_to_receive

  val balanceContainer: LinearLayout = cardWrap.findViewById(R.id.balanceContainer).asInstanceOf[LinearLayout]
  val balanceWalletFiat: TextView = cardWrap.findViewById(R.id.balanceWalletFiat).asInstanceOf[TextView]
  val balanceWallet: TextView = cardWrap.findViewById(R.id.balanceWallet).asInstanceOf[TextView]

  def onWalletTap: Unit
  def updateView: Unit
}

abstract class BtcWalletCard(host: BaseActivity, val xPub: ExtendedPublicKey) extends WalletCard(host) {
  imageTip.setImageResource(R.drawable.add_24)
  var isSelected: Boolean = false

  def updateView: Unit = {
    val spec = ElectrumWallet.specs(xPub)
    val hasMoney = spec.info.lastBalance.toLong > 0L
    val bgResource = if (isSelected) R.drawable.border_card_signing_on else R.color.cardBitcoinSigning
    infoWalletLabel setText spec.info.label.asSome.filter(_.trim.nonEmpty).getOrElse(host getString bitcoin_wallet)
    balanceWallet setText BtcDenom.parsedTT(spec.info.lastBalance.toMilliSatoshi, "#FFFFFF", signCardZero).html
    balanceWalletFiat setText WalletApp.currentMsatInFiatHuman(spec.info.lastBalance.toMilliSatoshi)
    host.setVisMany(hasMoney -> balanceContainer, !hasMoney -> imageTip)
    infoContainer setBackgroundResource bgResource
  }
}

abstract class UsdtWalletCard(host: BaseActivity, val xPriv: String) extends WalletCard(host) {
  infoContainer setBackgroundResource R.color.usdt
  imageTip.setImageResource(R.drawable.add_24)

  def updateView: Unit = {
    val info = WalletApp.linkUsdt.data.wallets.find(_.xPriv == xPriv).get
    infoWalletLabel setText info.label.asSome.filter(_.trim.nonEmpty).getOrElse(host getString usdt_wallet)
    balanceWallet setText Denomination.fiatTT(info.lastBalance, "0", "#FFFFFF", signCardZero, isIncoming = true).html
    host.setVisMany(info.isDust -> imageTip, !info.isDust -> balanceContainer, false -> balanceWalletFiat)
  }
}

abstract class TaWalletCard(host: BaseActivity) extends WalletCard(host) {
  lazy val activeLoans = host.getResources getStringArray R.array.ta_loans
  lazy val daysLeft = host.getResources getStringArray R.array.ta_days_left
  infoContainer setBackgroundResource R.drawable.border_gray
  infoWalletLabel setText ta_earn_label

  def updateView: Unit =
    WalletApp.linkClient.data match {
      case stat: LinkClient.UserStatus =>
        imageTip.setImageResource(R.drawable.info_24)
        val minDaysLeft = (stat.activeLoans.map(_.daysLeft) :+ 0L).minBy(identity)
        host.setVisMany(stat.activeLoans.nonEmpty -> balanceContainer, stat.activeLoans.isEmpty -> imageTip)
        balanceWallet setText WalletApp.app.plurOrZero(activeLoans, stat.activeLoans.size)
        balanceWalletFiat setText WalletApp.app.plurOrZero(daysLeft, minDaysLeft.toInt)
        infoWalletNotice setText stat.userName
      case LinkClient.LoggedOut =>
        imageTip.setImageResource(R.drawable.lock_24)
        host.setVisMany(false -> balanceContainer, true -> imageTip)
        infoWalletNotice setText ta_client_login
    }
}