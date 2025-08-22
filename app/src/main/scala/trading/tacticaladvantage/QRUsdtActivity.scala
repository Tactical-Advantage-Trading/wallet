package trading.tacticaladvantage

import android.os.Bundle
import android.widget.TextView
import trading.tacticaladvantage.BaseActivity.StringOps
import immortan.sqlite.CompleteUsdtWalletInfo
import trading.tacticaladvantage.R.string._
import trading.tacticaladvantage.utils._
import immortan.crypto.Tools._

class QRUsdtActivity extends QRActivity with ExternalDataChecker { me =>
  lazy private[this] val usdtQrCaption = findViewById(R.id.usdtQrCaption).asInstanceOf[TextView]
  lazy private[this] val usdtQr = new QRViewHolder(me findViewById R.id.usdtQr)
  private[this] var info: CompleteUsdtWalletInfo = _

  override def PROCEED(state: Bundle): Unit = {
    setContentView(R.layout.activity_qr_usdt)
    checkExternalData(noneRunnable)
  }

  override def checkExternalData(whenNone: Runnable): Unit = InputParser.checkAndMaybeErase {
    case data: CompleteUsdtWalletInfo => runAnd(info = data)(showQRCode)
    case _ => finish
  }

  def showQRCode: Unit = {
    val title = getString(dialog_receive_address) + "<br>" + info.label
    usdtQrCaption setText title.format(info.address.short0x).html
    usdtQr.qrLabel setText info.address.short0x.html

    runInFutureProcessOnUI(QRActivity.get(info.address, qrSize), onFail) { qrBitmap =>
      def share: Unit = runInFutureProcessOnUI(shareData(qrBitmap, info.address), onFail)(none)
      setVis(isVisible = false, usdtQr.qrEdit)

      usdtQr.qrCopy setOnClickListener onButtonTap(WalletApp.app copy info.address)
      usdtQr.qrCode setOnClickListener onButtonTap(WalletApp.app copy info.address)
      usdtQr.qrShare setOnClickListener onButtonTap(share)
      usdtQr.qrCode setImageBitmap qrBitmap
    }
  }
}
