package trading.tacticaladvantage

import android.os.Bundle
import android.view.ViewGroup
import android.widget.TextView
import androidx.appcompat.app.AlertDialog
import androidx.recyclerview.widget.RecyclerView
import com.azoft.carousellayoutmanager._
import fr.acinq.bitcoin.Btc
import fr.acinq.bitcoin.DeterministicWallet.ExtendedPublicKey
import fr.acinq.eclair._
import fr.acinq.eclair.blockchain.electrum.{ElectrumWallet, WalletSpec}
import immortan.crypto.Tools._
import immortan.utils.{BtcDenom, Denomination}
import trading.tacticaladvantage.BaseActivity.StringOps
import trading.tacticaladvantage.Colors._
import trading.tacticaladvantage.R.string._
import trading.tacticaladvantage.utils.{BitcoinUri, InputParser}

import scala.util.Success

class QRBtcActivity extends QRActivity with ExternalDataChecker { me =>
  lazy private[this] val chainQrCaption = findViewById(R.id.chainQrCaption).asInstanceOf[TextView]
  lazy private[this] val chainQrCodes = findViewById(R.id.chainQrCodes).asInstanceOf[RecyclerView]
  private[this] var addresses: List[BitcoinUri] = Nil
  private[this] var spec: WalletSpec = _

  val adapter: RecyclerView.Adapter[QRViewHolder] = new RecyclerView.Adapter[QRViewHolder] {
    override def onBindViewHolder(holder: QRViewHolder, pos: Int): Unit = updateView(addresses(pos), holder)
    override def getItemId(itemPosition: Int): Long = itemPosition
    override def getItemCount: Int = addresses.size

    override def onCreateViewHolder(parent: ViewGroup, viewType: Int): QRViewHolder = {
      val qrCodeContainer = getLayoutInflater.inflate(R.layout.frag_qr, parent, false)
      new QRViewHolder(qrCodeContainer)
    }

    private def updateView(bu: BitcoinUri, holder: QRViewHolder): Unit = bu.uri foreach { uri =>
      val humanAmountOpt = for (requestedAmount <- bu.amount) yield BtcDenom.parsedWithSignTT(requestedAmount, cardIn, cardZero)
      val contentToShare = if (bu.amount.isDefined || bu.label.isDefined) InputParser.bitcoin + InputParser.removePrefix(uri.toString) else bu.address

      val visibleText = (bu.label, humanAmountOpt) match {
        case Some(label) \ Some(amount) => s"${bu.address.short}<br><br>$label<br><br>$amount"
        case None \ Some(amount) => s"${bu.address.short}<br><br>$amount"
        case Some(label) \ None => s"${bu.address.short}<br><br>$label"
        case _ => bu.address.short
      }

      holder.qrLabel setText visibleText.html
      runInFutureProcessOnUI(QRActivity.get(contentToShare, qrSize), onFail) { qrBitmap =>
        def share: Unit = runInFutureProcessOnUI(shareData(qrBitmap, contentToShare), onFail)(none)
        holder.qrCopy setOnClickListener onButtonTap(WalletApp.app copy contentToShare)
        holder.qrCode setOnClickListener onButtonTap(WalletApp.app copy contentToShare)
        holder.qrEdit setOnClickListener onButtonTap(me editAddress bu)
        holder.qrShare setOnClickListener onButtonTap(share)
        holder.qrCode setImageBitmap qrBitmap
      }
    }
  }

  def editAddress(bu: BitcoinUri): Unit = {
    val maxMsat = Btc(21e6).toSatoshi.toMilliSatoshi
    val canReceiveFiatHuman = WalletApp.currentMsatInFiatHuman(maxMsat)
    val canReceiveHuman = BtcDenom.parsedWithSignTT(maxMsat, cardIn, cardZero)
    val body = getLayoutInflater.inflate(R.layout.frag_input_fiat_converter, null).asInstanceOf[ViewGroup]
    lazy val manager = new RateManager(body, getString(dialog_add_description).asSome, dialog_visibility_sender, WalletApp.fiatRates.info.rates, WalletApp.fiatCode)
    mkCheckForm(proceed, none, titleBodyAsViewBuilder(getString(dialog_receive_btc).asColoredView(R.color.cardBitcoinSigning), manager.content), dialog_ok, dialog_cancel)
    manager.hintFiatDenom.setText(getString(dialog_up_to).format(canReceiveFiatHuman).html)
    manager.hintDenom.setText(getString(dialog_up_to).format(canReceiveHuman).html)
    bu.amount.foreach(manager.updateText)

    def proceed(alert: AlertDialog): Unit = {
      val uriBuilder = bu.uri.get.buildUpon.clearQuery
      val resultMsat = manager.resultMsat.truncateToSatoshi.toMilliSatoshi
      val uriBuilder1 = if (resultMsat > ElectrumWallet.params.dustLimit) uriBuilder.appendQueryParameter("amount", Denomination.msat2BtcBigDecimal(resultMsat).toString) else uriBuilder
      val uriBuilder2 = manager.resultExtraInput match { case Some(resultExtraInput) => uriBuilder1.appendQueryParameter("label", resultExtraInput) case None => uriBuilder1 }

      addresses = addresses map {
        case oldUri if oldUri.address == bu.uri.get.getHost => BitcoinUri(Success(uriBuilder2.build), oldUri.address)
        case oldUri => BitcoinUri(oldUri.uri.map(_.buildUpon.clearQuery.build), oldUri.address)
      }

      adapter.notifyDataSetChanged
      alert.dismiss
    }
  }

  override def PROCEED(state: Bundle): Unit = {
    setContentView(R.layout.activity_qr_addresses)
    checkExternalData(noneRunnable)
  }

  def showQRCode: Unit = {
    val title = getString(dialog_receive_btc) + "<br>" + spec.info.label
    val keys = spec.data.firstUnusedAccountKeys.toList.sortBy(_.path.lastChildNumber)
    val layoutManager = new CarouselLayoutManager(CarouselLayoutManager.HORIZONTAL, false)
    layoutManager.setPostLayoutListener(new CarouselZoomPostLayoutListener)
    layoutManager.setMaxVisibleItems(ElectrumWallet.MAX_RECEIVE_ADDRESSES)

    // Allow MAX_RECEIVE_ADDRESSES - 16 (first 4 addresses) to be seen to not make it crowded
    addresses = keys.dropRight(16).map(spec.data.keys.ewt.textAddress).map(BitcoinUri.fromRaw)

    chainQrCaption.setText(title.html)
    chainQrCodes.addOnScrollListener(new CenterScrollListener)
    chainQrCodes.setLayoutManager(layoutManager)
    chainQrCodes.setHasFixedSize(true)
    chainQrCodes.setAdapter(adapter)
  }

  override def checkExternalData(whenNone: Runnable): Unit = InputParser.checkAndMaybeErase {
    case key: ExtendedPublicKey => runAnd(spec = ElectrumWallet specs key)(showQRCode)
    case _ => finish
  }
}
