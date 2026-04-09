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
import Tools._
import trading.tacticaladvantage.BaseActivity.StringOps
import trading.tacticaladvantage.R.string._
import trading.tacticaladvantage.utils.{CoinDenom, Denomination, InputParser, PlainCoinUri}

import scala.util.Success


class QRCoinActivity extends QRActivity with ExternalDataChecker { me =>
  lazy private[this] val chainQrCaption = findViewById(R.id.chainQrCaption).asInstanceOf[TextView]
  lazy private[this] val chainQrCodes = findViewById(R.id.chainQrCodes).asInstanceOf[RecyclerView]
  private[this] var addresses: List[PlainCoinUri] = Nil
  private[this] var group: NetworkWalletGroup = _
  private[this] var spec: WalletSpec = _

  val adapter: RecyclerView.Adapter[QRViewHolder] = new RecyclerView.Adapter[QRViewHolder] {
    override def onBindViewHolder(holder: QRViewHolder, pos: Int): Unit = updateView(addresses(pos), holder)
    override def getItemId(itemPosition: Int): Long = itemPosition
    override def getItemCount: Int = addresses.size

    override def onCreateViewHolder(parent: ViewGroup, viewType: Int): QRViewHolder = {
      val qrCodeContainer = getLayoutInflater.inflate(R.layout.frag_qr, parent, false)
      qrCodeContainer setBackgroundResource group.qrBgRes
      new QRViewHolder(qrCodeContainer)
    }

    private def updateView(cu: PlainCoinUri, holder: QRViewHolder): Unit = cu.uri foreach { uri =>
      val humanAmountOpt = for (requestedAmount <- cu.amount) yield CoinDenom.parsedTT(requestedAmount, cardIn, cardZero)
      val contentToShare = if (cu.amount.isDefined || cu.label.isDefined) group.prefix + InputParser.removePrefix(uri.toString) else cu.address

      val visibleText = (cu.label, humanAmountOpt) match {
        case Some(label) \ Some(amount) => s"${cu.address.short}<br><br>$label<br><br>$amount"
        case None \ Some(amount) => s"${cu.address.short}<br><br>$amount"
        case Some(label) \ None => s"${cu.address.short}<br><br>$label"
        case _ => cu.address.short
      }

      holder.qrLabel setText visibleText.html
      runInFutureProcessOnUI(QRActivity.get(contentToShare, qrSize), onFail) { qrBitmap =>
        def share: Unit = runInFutureProcessOnUI(shareData(qrBitmap, contentToShare), onFail)(none)
        holder.qrCopy setOnClickListener onButtonTap(WalletApp.app copy contentToShare)
        holder.qrCode setOnClickListener onButtonTap(WalletApp.app copy contentToShare)
        holder.qrEdit setOnClickListener onButtonTap(me editAddress cu)
        holder.qrShare setOnClickListener onButtonTap(share)
        holder.qrCode setImageBitmap qrBitmap
      }
    }
  }

  def editAddress(bu: PlainCoinUri): Unit = {
    val canReceiveHuman = CoinDenom.parsedTT(MAX_MSAT, cardIn, cardZero)
    val canReceiveFiatHuman = WalletApp.currentMsatInFiatHuman(group.fiatRates, MAX_MSAT)
    val body = getLayoutInflater.inflate(R.layout.frag_input_converter, null).asInstanceOf[LinearLayout]
    lazy val rm = new RateManager(new RateManagerContent(body), group, WalletApp.fiatCode)
    val title = getString(dialog_receive_address).asColoredView(group.bgRes)

    mkCheckForm(proceed, none, titleBodyAsViewBuilder(title, rm.rmc.container), dialog_ok, dialog_cancel)
    rm.rmc.hintFiatDenom.setText(getString(dialog_up_to).format(canReceiveFiatHuman).html)
    rm.rmc.hintDenom.setText(getString(dialog_up_to).format(canReceiveHuman).html)
    bu.amount.foreach(rm.updateText)

    def proceed(alert: AlertDialog): Unit = {
      val uriBuilder = bu.uri.get.buildUpon.clearQuery
      val resultMsat = rm.resultMsat.truncateToSatoshi.toMilliSatoshi
      val uriBuilder1 = if (resultMsat > group.electrum.params.dustLimit) {
        val amount = Denomination.msat2BtcBigDecimal(resultMsat).toString
        uriBuilder.appendQueryParameter("amount", amount)
      } else uriBuilder

      addresses = addresses map {
        case oldUri if oldUri.address == bu.uri.get.getHost => PlainCoinUri(Success(uriBuilder1.build), oldUri.address)
        case oldUri => PlainCoinUri(oldUri.uri.map(_.buildUpon.clearQuery.build), oldUri.address)
      }

      adapter.notifyDataSetChanged
      alert.dismiss
    }
  }

  override def PROCEED(state: Bundle): Unit = {
    setContentView(R.layout.activity_qr)
    checkExternalData(noneRunnable)
  }

  def showQRCode: Unit = {
    val title = getString(dialog_receive_address) + "<br>" + spec.info.label
    val keys = spec.data.firstUnusedAccountKeys.toList.sortBy(_.path.lastChildNumber)
    val layoutManager = new CarouselLayoutManager(CarouselLayoutManager.HORIZONTAL, false)
    layoutManager.setPostLayoutListener(new CarouselZoomPostLayoutListener)
    layoutManager.setMaxVisibleItems(ElectrumWallet.MAX_RECEIVE_ADDRESSES)

    // Allow MAX_RECEIVE_ADDRESSES - 16 (first 4 addresses) to be seen to not make it crowded
    addresses = keys.dropRight(16).map(spec.data.keys.ewt.textAddress).map(PlainCoinUri.fromRaw)

    chainQrCaption.setText(title.html)
    chainQrCodes.addOnScrollListener(new CenterScrollListener)
    chainQrCodes.setLayoutManager(layoutManager)
    chainQrCodes.setHasFixedSize(true)
    chainQrCodes.setAdapter(adapter)
  }

  override def checkExternalData(whenNone: Runnable): Unit = InputParser.checkAndMaybeErase {
    case (group1: NetworkWalletGroup, xPub: ExtendedPublicKey) if group1.electrum.specs.contains(xPub) =>
      spec = group1.electrum.specs(xPub)
      group = group1
      showQRCode
    case _ =>
      finish
  }
}
