package trading.tacticaladvantage

import android.os.Bundle
import android.view.ViewGroup
import android.widget.{LinearLayout, TextView}
import androidx.appcompat.app.AlertDialog
import androidx.recyclerview.widget.RecyclerView
import com.azoft.carousellayoutmanager._
import fr.acinq.bitcoin.DeterministicWallet.ExtendedPublicKey
import fr.acinq.eclair._
import fr.acinq.eclair.blockchain.electrum.{ElectrumWallet, WalletSpec}
import immortan.Tools._
import immortan.utils.{BtcDenom, Denomination}
import trading.tacticaladvantage.BaseActivity.StringOps
import trading.tacticaladvantage.Colors._
import trading.tacticaladvantage.R.string._
import trading.tacticaladvantage.utils.{InputParser, PlainBitcoinUri}

import scala.util.Success

class QRBtcActivity extends QRActivity with ExternalDataChecker { me =>
  lazy private[this] val chainQrCaption = findViewById(R.id.chainQrCaption).asInstanceOf[TextView]
  lazy private[this] val chainQrCodes = findViewById(R.id.chainQrCodes).asInstanceOf[RecyclerView]
  private[this] var addresses: List[PlainBitcoinUri] = Nil
  private[this] var spec: WalletSpec = _

  val adapter: RecyclerView.Adapter[QRViewHolder] = new RecyclerView.Adapter[QRViewHolder] {
    override def onBindViewHolder(holder: QRViewHolder, pos: Int): Unit = updateView(addresses(pos), holder)
    override def getItemId(itemPosition: Int): Long = itemPosition
    override def getItemCount: Int = addresses.size

    override def onCreateViewHolder(parent: ViewGroup, viewType: Int): QRViewHolder = {
      val qrCodeContainer = getLayoutInflater.inflate(R.layout.frag_qr, parent, false)
      new QRViewHolder(qrCodeContainer)
    }

    private def updateView(pbu: PlainBitcoinUri, holder: QRViewHolder): Unit = pbu.uri foreach { uri =>
      val humanAmountOpt = for (requestedAmount <- pbu.amount) yield BtcDenom.parsedTT(requestedAmount, cardIn, cardZero)
      val contentToShare = if (pbu.amount.isDefined || pbu.label.isDefined) InputParser.bitcoin + InputParser.removePrefix(uri.toString) else pbu.address

      val visibleText = (pbu.label, humanAmountOpt) match {
        case Some(label) \ Some(amount) => s"${pbu.address.short}<br><br>$label<br><br>$amount"
        case None \ Some(amount) => s"${pbu.address.short}<br><br>$amount"
        case Some(label) \ None => s"${pbu.address.short}<br><br>$label"
        case _ => pbu.address.short
      }

      holder.qrLabel setText visibleText.html
      runInFutureProcessOnUI(QRActivity.get(contentToShare, qrSize), onFail) { qrBitmap =>
        def share: Unit = runInFutureProcessOnUI(shareData(qrBitmap, contentToShare), onFail)(none)
        holder.qrCopy setOnClickListener onButtonTap(WalletApp.app copy contentToShare)
        holder.qrCode setOnClickListener onButtonTap(WalletApp.app copy contentToShare)
        holder.qrEdit setOnClickListener onButtonTap(me editAddress pbu)
        holder.qrShare setOnClickListener onButtonTap(share)
        holder.qrCode setImageBitmap qrBitmap
      }
    }
  }

  def editAddress(bu: PlainBitcoinUri): Unit = {
    val canReceiveHuman = BtcDenom.parsedTT(MAX_MSAT, cardIn, cardZero)
    val canReceiveFiatHuman = WalletApp.currentMsatInFiatHuman(MAX_MSAT)
    val body = getLayoutInflater.inflate(R.layout.frag_input_converter, null).asInstanceOf[LinearLayout]
    val title = getString(dialog_receive_address).asColoredView(R.color.cardBitcoinSigning)
    lazy val rm = new RateManager(new RateManagerContent(body), WalletApp.fiatCode)

    mkCheckForm(proceed, none, titleBodyAsViewBuilder(title, rm.rmc.container), dialog_ok, dialog_cancel)
    rm.rmc.hintFiatDenom.setText(getString(dialog_up_to).format(canReceiveFiatHuman).html)
    rm.rmc.hintDenom.setText(getString(dialog_up_to).format(canReceiveHuman).html)
    bu.amount.foreach(rm.updateText)

    def proceed(alert: AlertDialog): Unit = {
      val uriBuilder = bu.uri.get.buildUpon.clearQuery
      val resultMsat = rm.resultMsat.truncateToSatoshi.toMilliSatoshi
      val uriBuilder1 = if (resultMsat > ElectrumWallet.params.dustLimit) {
        val amount = Denomination.msat2BtcBigDecimal(resultMsat).toString
        uriBuilder.appendQueryParameter("amount", amount)
      } else uriBuilder

      addresses = addresses map {
        case oldUri if oldUri.address == bu.uri.get.getHost => PlainBitcoinUri(Success(uriBuilder1.build), oldUri.address)
        case oldUri => PlainBitcoinUri(oldUri.uri.map(_.buildUpon.clearQuery.build), oldUri.address)
      }

      adapter.notifyDataSetChanged
      alert.dismiss
    }
  }

  override def PROCEED(state: Bundle): Unit = {
    setContentView(R.layout.activity_qr_btc)
    checkExternalData(noneRunnable)
  }

  def showQRCode: Unit = {
    val title = getString(dialog_receive_address) + "<br>" + spec.info.label
    val keys = spec.data.firstUnusedAccountKeys.toList.sortBy(_.path.lastChildNumber)
    val layoutManager = new CarouselLayoutManager(CarouselLayoutManager.HORIZONTAL, false)
    layoutManager.setPostLayoutListener(new CarouselZoomPostLayoutListener)
    layoutManager.setMaxVisibleItems(ElectrumWallet.MAX_RECEIVE_ADDRESSES)

    // Allow MAX_RECEIVE_ADDRESSES - 16 (first 4 addresses) to be seen to not make it crowded
    addresses = keys.dropRight(16).map(spec.data.keys.ewt.textAddress).map(PlainBitcoinUri.fromRaw)

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
