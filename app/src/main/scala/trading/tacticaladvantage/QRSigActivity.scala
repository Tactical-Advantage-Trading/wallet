package trading.tacticaladvantage

import android.os.Bundle
import android.widget.TextView
import trading.tacticaladvantage.BaseActivity.StringOps
import trading.tacticaladvantage.utils._
import immortan.Tools._

class QRSigActivity extends QRActivity with ExternalDataChecker { me =>
  lazy private[this] val sigQrCaption = findViewById(R.id.sigQrCaption).asInstanceOf[TextView]
  lazy private[this] val sigQr = new QRViewHolder(me findViewById R.id.sigQr)
  private[this] var bip322Data: BIP322VerifyData = _

  override def PROCEED(state: Bundle): Unit = {
    setContentView(R.layout.activity_qr_signature)
    checkExternalData(noneRunnable)
  }

  override def checkExternalData(whenNone: Runnable): Unit = InputParser.checkAndMaybeErase {
    case data: BIP322VerifyData => runAnd(bip322Data = data)(showQRCode)
    case _ => finish
  }

  def showQRCode: Unit = {
    val title = getString(R.string.sign_signed_message_title)
    sigQrCaption setText title.format(bip322Data.address.short).html

    runInFutureProcessOnUI(QRActivity.get(bip322Data.serialize, qrSize), onFail) { qrBitmap =>
      def share: Unit = runInFutureProcessOnUI(shareData(qrBitmap, bip322Data.serialize), onFail)(none)
      setVisMany(false -> sigQr.qrEdit, false -> sigQr.qrLabel)

      sigQr.qrCopy setOnClickListener onButtonTap(WalletApp.app copy bip322Data.serialize)
      sigQr.qrCode setOnClickListener onButtonTap(WalletApp.app copy bip322Data.serialize)
      sigQr.qrShare setOnClickListener onButtonTap(share)
      sigQr.qrCode setImageBitmap qrBitmap
    }
  }
}
