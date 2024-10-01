package com.btcontract.wallet

import android.os.Bundle
import android.view.{View, ViewGroup}
import android.widget._
import androidx.cardview.widget.CardView
import androidx.recyclerview.widget.RecyclerView
import androidx.transition.TransitionManager
import com.btcontract.wallet.BaseActivity.StringOps
import com.btcontract.wallet.Colors.{cardIn, cardOut, cardZero}
import com.btcontract.wallet.R.string._
import com.btcontract.wallet.utils.InputParser
import fr.acinq.bitcoin.DeterministicWallet.ExtendedPublicKey
import fr.acinq.bitcoin._
import fr.acinq.eclair._
import fr.acinq.eclair.blockchain.electrum.ElectrumWallet.WalletReady
import fr.acinq.eclair.blockchain.electrum.{ElectrumWallet, Utxo, WalletSpec}
import immortan.TxDescription
import immortan.crypto.Tools._
import immortan.utils._


class CoinControlActivity extends BaseCheckActivity with ExternalDataChecker { me =>
  lazy val coinControlContainer = findViewById(R.id.coinControlContainer).asInstanceOf[LinearLayout]
  lazy val utxoList = findViewById(R.id.utxoList).asInstanceOf[ListView]

  trait UtxoListItem
  case class TransactionLine(txid: String) extends UtxoListItem
  case class UnspentOutputLine(utxo: Utxo) extends UtxoListItem

  private[this] var spec: WalletSpec = _
  private[this] var chooser: ChainWalletCards = _
  private[this] var txLabels: Map[String, TxDescription] = Map.empty
  private[this] var excludedOutPoints: Set[OutPoint] = Set.empty
  private[this] var items: List[UtxoListItem] = List.empty

  private val chainListener = new WalletEventsListener {
    override def onWalletReady(event: WalletReady): Unit = UITask {
      excludedOutPoints = event.excludedOutPoints.toSet
      updateItems(event.unExcludedUtxos)
      chanAdapter.notifyDataSetChanged
      updateWallet
    }.run
  }

  val chanAdapter: BaseAdapter = new BaseAdapter {
    override def getItem(pos: Int): UtxoListItem = items(pos)
    override def getItemId(position: Int): Long = position
    override def getCount: Int = items.size

    def getView(position: Int, savedView: View, parent: ViewGroup): View = {
      val view = if (null == savedView) getLayoutInflater.inflate(R.layout.frag_utxo_line, null) else savedView.asInstanceOf[View]
      val holder = if (null == view.getTag) new UtxoHolder(view) else view.getTag.asInstanceOf[UtxoHolder]

      getItem(position) match {
        case item: TransactionLine => holder.setTxView(item)
        case item: UnspentOutputLine => holder.setUtxoView(item)
      }

      view
    }
  }

  class UtxoHolder(itemView: View) extends RecyclerView.ViewHolder(itemView) {
    val txLabelOrId: TextView = itemView.findViewById(R.id.txLabelOrId).asInstanceOf[TextView]
    val utxoWrap: RelativeLayout = itemView.findViewById(R.id.utxoWrap).asInstanceOf[RelativeLayout]
    val utxoCardContainer: CardView = itemView.findViewById(R.id.utxoCardContainer).asInstanceOf[CardView]

    val utxoIncluded: CheckBox = itemView.findViewById(R.id.utxoIncluded).asInstanceOf[CheckBox]
    val utxoHaikuName: TextView = itemView.findViewById(R.id.utxoHaikuName).asInstanceOf[TextView]
    val utxoAmount: TextView = itemView.findViewById(R.id.utxoAmount).asInstanceOf[TextView]
    itemView.setTag(this)

    def setTxView(item: TransactionLine): Unit = {
      setVisMany(true -> txLabelOrId, false -> utxoWrap)
      val labelOpt = txLabels.get(item.txid).flatMap(_.label)
      txLabelOrId setOnClickListener onButtonTap(WalletApp.app copy item.txid)
      txLabelOrId setText labelOpt.getOrElse(s"TXID ${item.txid.short}".html)
    }

    def setUtxoView(item: UnspentOutputLine): Unit = {
      setVisMany(false -> txLabelOrId, true -> utxoWrap)
      val amount = item.utxo.item.value.sat.toMilliSatoshi
      val humanAmount = WalletApp.denom.directedWithSign(amount, 0L.msat, cardOut, cardIn, cardZero, isIncoming = true)
      val isExcluded = excludedOutPoints.contains(item.utxo.item.outPoint)
      val utxoName = Haiku.name(item.utxo.key.publickeybytes)

      utxoCardContainer setOnClickListener onButtonTap {
        val excludedOutPoints1 = if (utxoIncluded.isChecked) excludedOutPoints + item.utxo.item.outPoint else excludedOutPoints - item.utxo.item.outPoint
        ElectrumWallet.specs(spec.data.keys.ewt.xPub).walletRef ! ElectrumWallet.SetExcludedOutPoints(excludedOutPoints1.toList)
      }

      utxoAmount.setText(humanAmount.html)
      utxoIncluded.setChecked(!isExcluded)
      utxoHaikuName.setText(utxoName)
    }
  }

  def updateItems(unExcludedUtxos: Seq[Utxo] = Nil): Unit = {
    items = unExcludedUtxos.groupBy(_.item.outPoint.txid.toString).flatMap { case (txid, unspents) =>
      val outPointsLines = for (unspentOutput <- unspents) yield UnspentOutputLine(unspentOutput)
      TransactionLine(txid) +: outPointsLines
    }.toList
  }

  override def onDestroy: Unit = {
    val remove = WalletEventsCatcher.Remove(chainListener)
    try ElectrumWallet.catcher ! remove catch none
    super.onDestroy
  }

  override def PROCEED(state: Bundle): Unit = {
    setContentView(R.layout.activity_coin_control)
    checkExternalData(noneRunnable)
  }

  def updateWallet: Unit = {
    TransitionManager.beginDelayedTransition(chooser.holder)
    // We can't use a spec directly here because we have an old copy
    chooser.update(ElectrumWallet.specs.get(spec.data.keys.ewt.xPub).toList)
    chooser.unPadCards
  }

  def showWalletInfo: Unit = {
    chooser = new ChainWalletCards(me) {
      val holder: LinearLayout = findViewById(R.id.chainCardContainer).asInstanceOf[LinearLayout]
      override def onWalletTap(key: ExtendedPublicKey): Unit = goToWithValue(ClassNames.qrChainActivityClass, key)
    }

    txLabels = WalletApp.txDataBag.listAllDescriptions
    ElectrumWallet.catcher ! chainListener
    chooser.init(1)
    updateWallet

    utxoList.setAdapter(chanAdapter)
    utxoList.setDividerHeight(0)
    utxoList.setDivider(null)

    coinControlContainer.addView(new TitleView(me getString coin_control).view, 0)
    spec.data.lastReadyMessage.foreach(chainListener.onWalletReady)
  }

  override def checkExternalData(whenNone: Runnable): Unit = InputParser.checkAndMaybeErase {
    case key: ExtendedPublicKey => runAnd(spec = ElectrumWallet specs key)(showWalletInfo)
    case _ => finish
  }
}
