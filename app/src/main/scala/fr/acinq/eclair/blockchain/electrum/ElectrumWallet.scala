package fr.acinq.eclair.blockchain.electrum

import java.util.concurrent.ConcurrentHashMap
import akka.actor.{Actor, ActorRef, ActorSystem, PoisonPill, Props}
import akka.pattern.ask
import akka.util.Timeout
import fr.acinq.bitcoin.DeterministicWallet._
import fr.acinq.bitcoin._
import fr.acinq.eclair.blockchain.electrum.Blockchain.RETARGETING_PERIOD
import fr.acinq.eclair.blockchain.electrum.ElectrumClient._
import fr.acinq.eclair.blockchain.electrum.db.sqlite.SqliteWalletDb.persistentDataCodec
import fr.acinq.eclair.blockchain.fee.FeeratePerKw
import fr.acinq.eclair.{MilliSatoshi, addressToPublicKeyScript}
import immortan.{CanBeShutDown, ConnectionProvider}
import immortan.Tools._
import immortan.sqlite.{CompleteBtcWalletInfo, SQLiteBtcTx, SQLiteBtcWallet, SQLiteData, SigningWallet}
import scodec.bits.ByteVector

import scala.annotation.tailrec
import scala.collection.JavaConverters._
import scala.collection.{immutable, mutable}
import scala.concurrent.duration._
import scala.concurrent.{ExecutionContextExecutor, Future}
import scala.math.min
import scala.util.Try

object ElectrumWallet extends CanBeShutDown {
  type OkOrError = Option[fr.acinq.eclair.blockchain.bitcoind.rpc.Error]
  type TxHistoryItemList = List[TransactionHistoryItem]
  type TxOutOption = Option[TxOut]

  var connectionProvider: ConnectionProvider = _
  var params: WalletParameters = _
  var chainHash: ByteVector32 = _
  var catcher: ActorRef = _
  var sync: ActorRef = _
  var pool: ActorRef = _

  val specs: mutable.Map[ExtendedPublicKey, WalletSpec] =
    new ConcurrentHashMap[ExtendedPublicKey, WalletSpec].asScala

  implicit val timeout: Timeout = Timeout(1.minute)
  implicit val system: ActorSystem = ActorSystem("immortan-actor-system")
  implicit val ec: ExecutionContextExecutor = scala.concurrent.ExecutionContext.Implicits.global
  def addressToPubKeyScript(address: String): ByteVector = Script write addressToPublicKeyScript(address, chainHash)

  final val OPT_IN_FULL_RBF = TxIn.SEQUENCE_FINAL - 2
  final val MAX_RECEIVE_ADDRESSES = 20

  final val BIP32 = "BIP32"
  final val BIP44 = "BIP44"
  final val BIP49 = "BIP49"
  final val BIP84 = "BIP84"

  final val KEY_REFILL = "key-refill"
  final val PARENTS_MISSING = 1
  final val FOREIGN_INPUTS = 2

  sealed trait State
  case object DISCONNECTED extends State
  case object WAITING_FOR_TIP extends State
  case object SYNCING extends State
  case object RUNNING extends State

  sealed trait Request
  case class ChainFor(target: ActorRef) extends Request
  case class SetExcludedOutPoints(outPoints: List[OutPoint] = Nil) extends Request

  sealed trait Response
  sealed trait GenerateTxResponse extends Response {
    // This is guaranteed to exclude our own change output
    lazy val transferred: Satoshi = pubKeyScriptToAmount.values.sum
    val pubKeyScriptToAmount: Map[ByteVector, Satoshi]
    // Order matches each related signed tx input
    val usedWallets: Seq[ElectrumWalletType]
    val tx: Transaction
    val fee: Satoshi
  }

  case class CompleteTransactionResponse(pubKeyScriptToAmount: Map[ByteVector, Satoshi], usedWallets: Seq[ElectrumWalletType], tx: Transaction, fee: Satoshi) extends GenerateTxResponse
  case class SendAllResponse(pubKeyScriptToAmount: Map[ByteVector, Satoshi], usedWallets: Seq[ElectrumWalletType], tx: Transaction, fee: Satoshi) extends GenerateTxResponse
  case class RBFResponse(result: Either[Int, GenerateTxResponse] = PARENTS_MISSING.asLeft) extends Response
  case class IsDoubleSpentResponse(depth: Long, stamp: Long, isDoubleSpent: Boolean) extends Response

  sealed trait WalletEvent
  case class WalletReady(balance: Satoshi, height: Long, heightsCode: Int, xPub: ExtendedPublicKey, unExcludedUtxos: Seq[Utxo], excludedOutPoints: List[OutPoint] = Nil) extends WalletEvent
  case class TransactionReceived(tx: Transaction, depth: Long, stamp: Long, received: Satoshi, sent: Satoshi, addresses: StringList, xPubs: List[ExtendedPublicKey] = Nil) extends WalletEvent {
    def merge(that: TransactionReceived) = TransactionReceived(tx, min(depth, that.depth), min(stamp, that.stamp), received + that.received, sent + that.sent, addresses ++ that.addresses, xPubs ++ that.xPubs)
  }

  override def becomeShutDown: Unit = {
    val actors = List(catcher, sync, pool)
    val walletRefs = specs.values.map(_.walletRef)
    for (actor <- walletRefs ++ actors) actor ! PoisonPill
  }

  def weight2feeMsat(feeratePerKw: FeeratePerKw, weight: Int): MilliSatoshi = MilliSatoshi(feeratePerKw.toLong * weight)
  def weight2fee(feeratePerKw: FeeratePerKw, weight: Int): Satoshi = weight2feeMsat(feeratePerKw, weight).truncateToSatoshi
  def completeTransaction(tx: Transaction, feeRatePerKw: FeeratePerKw, sequenceFlag: Long, changeScript: Seq[ScriptElt],
                          usableInUtxos: Seq[Utxo], mustUseUtxos: Seq[Utxo] = Nil): CompleteTransactionResponse = {

    def computeFee(candidateUtxos: Seq[Utxo], change: TxOutOption) = {
      val tx1 = ElectrumWalletType.dummySignTransaction(candidateUtxos, tx, sequenceFlag)
      val weight = change.map(tx1.addOutput).getOrElse(tx1).weight(Protocol.PROTOCOL_VERSION)
      weight2fee(feeRatePerKw, weight)
    }

    val amountToSend = tx.txOut.map(_.amount).sum
    val changeTxOut = TxOut(Satoshi(0L), changeScript)

    require(tx.txIn.isEmpty, "Cannot complete a tx that already has inputs")
    require(amountToSend > params.dustLimit, "Amount to send is below dust limit")

    @tailrec
    def loop(current: Seq[Utxo], remaining: Seq[Utxo] = Nil): (Seq[Utxo], TxOutOption) = current.map(_.item.value).sum.sat match {
      case total if total - computeFee(current, None) < amountToSend && remaining.isEmpty => throw new RuntimeException("Insufficient funds")
      case total if total - computeFee(current, None) < amountToSend => loop(remaining.head +: current, remaining.tail)

      case total if total - computeFee(current, None) <= amountToSend + params.dustLimit => (current, None)
      case total if total - computeFee(current, changeTxOut.asSome) <= amountToSend + params.dustLimit && remaining.isEmpty => (current, None)
      case total if total - computeFee(current, changeTxOut.asSome) <= amountToSend + params.dustLimit => loop(remaining.head +: current, remaining.tail)
      case total => (current, changeTxOut.copy(amount = total - computeFee(current, changeTxOut.asSome) - amountToSend).asSome)
    }

    val usable = usableInUtxos.sortBy(_.item.value)
    val (selected, changeOpt) = loop(current = mustUseUtxos, usable)
    val txWithInputs = ElectrumWalletType.dummySignTransaction(selected, tx, sequenceFlag)
    val txWithChange = changeOpt.map(txWithInputs.addOutput).getOrElse(txWithInputs)

    val (wallets, tx3) = ElectrumWalletType.signTransaction(usableInUtxos ++ mustUseUtxos, txWithChange)
    val computedFee = selected.map(_.item.value).sum.sat - tx3.txOut.map(_.amount).sum
    CompleteTransactionResponse(Map.empty, wallets, tx3, computedFee)
  }

  def spendAll(restPubKeyScript: ByteVector, strictPubKeyScriptsToAmount: Map[ByteVector, Satoshi],
               usableInUtxos: Seq[Utxo], feeRatePerKw: FeeratePerKw, sequenceFlag: Long,
               extraOutUtxos: List[TxOut] = Nil): SendAllResponse = {

    val strictTxOuts = for (pubKeyScript \ amount <- strictPubKeyScriptsToAmount) yield TxOut(amount, pubKeyScript)
    val restTxOut = TxOut(amount = usableInUtxos.map(_.item.value.sat).sum - strictTxOuts.map(_.amount).sum - extraOutUtxos.map(_.amount).sum, restPubKeyScript)
    val tx = Transaction(version = 2, txIn = Nil, txOut = restTxOut :: strictTxOuts.toList ::: extraOutUtxos, lockTime = 0)
    val tx1 = ElectrumWalletType.dummySignTransaction(usableInUtxos, tx, sequenceFlag)

    val fee = weight2fee(weight = tx1.weight(Protocol.PROTOCOL_VERSION), feeratePerKw = feeRatePerKw)
    require(restTxOut.amount - fee > params.dustLimit, "Resulting tx amount to send is below dust limit")

    val restTxOut1 = TxOut(restTxOut.amount - fee, restPubKeyScript)
    val tx2 = tx1.copy(txOut = restTxOut1 :: strictTxOuts.toList ::: extraOutUtxos)
    val allPubKeyScriptsToAmount = strictPubKeyScriptsToAmount.updated(restPubKeyScript, restTxOut1.amount)
    val (wallets, tx3) = ElectrumWalletType.signTransaction(usableInUtxos, tx2)
    SendAllResponse(allPubKeyScriptsToAmount, wallets, tx3, fee)
  }

  def computeTxDelta(datas: Seq[ElectrumData], transaction: Transaction): Option[TransactionDelta] = {
    // For incoming transactions this will work because they typically have no our inputs at all
    // For outgoing transactions this wiil work only if all inputs come from our utxos

    val ourOutputs = for {
      txOutput <- transaction.txOut
      data <- datas if data.isMine(txOutput)
    } yield txOutput

    val ourInputs = for {
      txInput <- transaction.txIn
      data <- datas if data.extPubKeyFromInput(txInput).isDefined
    } yield txInput

    val utxos = for {
      txInput <- ourInputs
      data <- datas if data.transactions.contains(txInput.outPoint.txid)
      txOutput = data.transactions(txInput.outPoint.txid).txOut(txInput.outPoint.index.toInt)
      item = UnspentItem(txInput.outPoint.txid, txInput.outPoint.index.toInt, txOutput.amount.toLong, 0)
      spentPubKeyExt <- data.keys.publicScriptMap.get(txOutput.publicKeyScript)
    } yield Utxo(spentPubKeyExt, item, data.keys.ewt)

    if (utxos.size != ourInputs.size) None else {
      val totalMineSent = utxos.map(_.item.value.sat).sum
      val totalMineReceived = ourOutputs.map(_.amount).sum
      TransactionDelta(utxos, totalMineReceived, totalMineSent).asSome
    }
  }

  // API

  // scriptToAmount is empty if we send all, empty then updated to a single item if we send to an address, already non-empty if we batch
  def makeBatchTx(specs: Seq[WalletSpec], changeTo: WalletSpec, scriptToAmount: Map[ByteVector, Satoshi], feeRatePerKw: FeeratePerKw): GenerateTxResponse = {
    val changeScript = changeTo.data.keys.ewt.computePublicKeyScript(changeTo.data.firstUnusedChangeKeys.headOption.getOrElse(changeTo.data.keys.changeKeys.head).publicKey)
    val tx = Transaction(version = 2, txIn = Nil, txOut = for (script \ amount <- scriptToAmount.toList) yield TxOut(amount, script), lockTime = 0L)
    val result = completeTransaction(tx, feeRatePerKw, OPT_IN_FULL_RBF, changeScript, specs.flatMap(_.data.utxos), Nil)
    result.copy(pubKeyScriptToAmount = scriptToAmount)
  }

  def makeTx(specs: Seq[WalletSpec], changeTo: WalletSpec, pubKeyScript: ByteVector, amount: Satoshi, prevScriptToAmount: Map[ByteVector, Satoshi], feeRatePerKw: FeeratePerKw): GenerateTxResponse = {
    if (specs.map(_.data.balance).sum != prevScriptToAmount.values.sum + amount) makeBatchTx(specs, changeTo, prevScriptToAmount.updated(pubKeyScript, amount), feeRatePerKw)
    else spendAll(pubKeyScript, prevScriptToAmount, specs.flatMap(_.data.utxos), feeRatePerKw, OPT_IN_FULL_RBF)
  }

  private def emptyUtxo(pubKeyScript: ByteVector): TxOut = TxOut(Satoshi(0L), pubKeyScript)
  def stampHashes(utxo: Utxo, hashes: Set[ByteVector32], pubKeyScript: ByteVector, feeRatePerKw: FeeratePerKw): GenerateTxResponse = {
    val preimageTxOuts = hashes.toList.map(_.bytes).map(OP_PUSHDATA.apply).grouped(2).map(OP_RETURN :: _).map(Script.write).map(emptyUtxo).toList
    spendAll(pubKeyScript, Map.empty, List(utxo), feeRatePerKw, OPT_IN_FULL_RBF, preimageTxOuts)
  }

  def makeCPFP(specs: Seq[WalletSpec], fromOutpoints: Set[OutPoint], pubKeyScript: ByteVector, feeRatePerKw: FeeratePerKw): GenerateTxResponse = {
    // We might select one largest UTXO to be CPFPd but most often we will only have one anyway and we can run into fee bump issues with less usable UTXOs
    val usableInUtxos = specs.flatMap(_.data.utxos).filter(utxo => fromOutpoints contains utxo.item.outPoint)
    spendAll(pubKeyScript, Map.empty, usableInUtxos, feeRatePerKw, OPT_IN_FULL_RBF)
  }

  def rbfBump(specs: Seq[WalletSpec], changeTo: WalletSpec, transaction: Transaction, feeRatePerKw: FeeratePerKw): RBFResponse = {
    // We could have sent everything to a certain address, in which case we can deduce a higher fee from receiver
    // Or we have sent such that we have change, in which case a higher fee must be deduced from sender
    val usedDatas = specs.map(_.data)

    val myOutputs = for {
      txOutput <- transaction.txOut
      data <- usedDatas if data.isMine(txOutput)
    } yield txOutput

    val foreignOutputs = transaction.txOut diff myOutputs
    val tx1 = transaction.copy(txOut = foreignOutputs, txIn = Nil)

    computeTxDelta(usedDatas, transaction) match {
      // We sent ALL our funds from given wallets to a single address which does not belong to us
      case Some(delta) if transaction.txOut.size == 1 && tx1.txOut.nonEmpty && usedDatas.flatMap(_.utxos).isEmpty =>
        RBFResponse(spendAll(tx1.txOut.head.publicKeyScript, Map.empty, delta.spentUtxos, feeRatePerKw, OPT_IN_FULL_RBF).asRight)

      case Some(delta) =>
        val leftUtxos = usedDatas.flatMap(_.utxos).filterNot(_.item.txHash == transaction.txid)
        // We have sent an amount with fee, this overwhelmingly likely means there's a change which got back to our wallet, we must use that leftover to bump the fee
        val changeScript = changeTo.data.keys.ewt.computePublicKeyScript(changeTo.data.firstUnusedChangeKeys.headOption.getOrElse(changeTo.data.keys.changeKeys.head).publicKey)
        RBFResponse(completeTransaction(tx1, feeRatePerKw, OPT_IN_FULL_RBF, changeScript, leftUtxos, delta.spentUtxos).asRight)

      case None => RBFResponse(PARENTS_MISSING.asLeft)
      case _ => RBFResponse(FOREIGN_INPUTS.asLeft)
    }
  }

  // We use this one for cancelling (rerouting onto our own address)
  def rbfReroute(pubs: Seq[WalletSpec], tx: Transaction, feeRatePerKw: FeeratePerKw, publicKeyScript: ByteVector): RBFResponse = computeTxDelta(pubs.map(_.data), tx) match {
    case Some(delta) => RBFResponse(spendAll(publicKeyScript, Map.empty, usableInUtxos = delta.spentUtxos, feeRatePerKw, OPT_IN_FULL_RBF).asRight)
    case None => RBFResponse(PARENTS_MISSING.asLeft)
  }

  def doubleSpent(pub: ExtendedPublicKey, tx: Transaction): IsDoubleSpentResponse = {
    // Transaction is considered double spent if we have overridden it by another and it got confirmed
    // or if somehow we know nothing about tx (it may be removed locally due to remote RBF-cancel)
    val data = specs(pub).data

    val doubleSpendTrials = for {
      spendingTxid <- data.overriddenPendingTxids.get(tx.txid)
      spendingBlockHeight <- data.proofs.get(spendingTxid).map(_.blockHeight)
    } yield data.computeDepth(spendingBlockHeight) > 0

    val depth = data.depth(tx.txid)
    val stamp = data.timestamp(tx.txid, params.headerDb)
    // Has been replaced by one of our txs or somehow we know nothing about it
    val isDoubleSpent = doubleSpendTrials.contains(true) || !data.isTxKnown(tx.txid)
    IsDoubleSpentResponse(depth, stamp, isDoubleSpent)
  }

  def orderByImportance(candidates: Seq[WalletSpec] = Nil): Seq[WalletSpec] = candidates.sortBy {
    case hardware if hardware.data.keys.ewt.secrets.isEmpty && hardware.info.core.masterFingerprint.nonEmpty => 0
    case default if default.data.keys.ewt.secrets.nonEmpty && default.info.core.walletType == BIP84 => 1
    case signing if signing.data.keys.ewt.secrets.nonEmpty => 2
    case _ => 3
  }

  // Async API

  def broadcast(tx: Transaction): Future[OkOrError] = pool ? BroadcastTransaction(tx) flatMap {
    case ElectrumClient.ServerError(_: ElectrumClient.BroadcastTransaction, error) => Future(error.asSome)
    case res: ElectrumClient.BroadcastTransactionResponse if res.error.isDefined => Future(res.error)
    case _ => Future(None)
  }

  // Wallet management API

  def makeSigningWalletParts(core: SigningWallet, ewt: ElectrumWalletType, lastBalance: Satoshi, label: String): WalletSpec = {
    val info = CompleteBtcWalletInfo(core, initData = ByteVector.empty, lastBalance, label, isCoinControlOn = false)
    val walletRef = system.actorOf(Props(classOf[ElectrumWallet], pool, sync, ewt), ewt.xPub.publicKey.toString)
    WalletSpec(info, ElectrumData(keys = MemoizedKeys(ewt), blockchain = null), walletRef)
  }

  def removeWallet(key: ExtendedPublicKey): Unit = {
    specs.remove(key).foreach(_.walletRef ! PoisonPill)
    params.walletDb.remove(key.publicKey)
  }

  def setLabel(newLabel: String)(key: ExtendedPublicKey): Unit = {
    ElectrumWallet.params.walletDb.updateLabel(newLabel, key.publicKey)
    specs.update(key, specs(key) withNewLabel newLabel)
  }
}

class ElectrumWallet(client: ActorRef, chainSync: ActorRef, ewt: ElectrumWalletType) extends Actor {
  import fr.acinq.eclair.blockchain.electrum.ElectrumWallet._

  context.system.eventStream.subscribe(channel = classOf[Blockchain], subscriber = self)
  client ! ElectrumClient.AddStatusListener(self)
  var state: State = DISCONNECTED

  def persistAndNotify(data1: ElectrumData): Unit = {
    context.system.scheduler.scheduleOnce(delay = 100.millis, self, KEY_REFILL)(context.system.dispatcher)
    val info1 = specs(ewt.xPub).info.copy(lastBalance = data1.balance, isCoinControlOn = data1.excludedOutPoints.nonEmpty)
    val spec1 = WalletSpec(info1, data1, self)

    if (data1.lastReadyMessage contains data1.currentReadyMessage) specs.update(ewt.xPub, spec1) else {
      val spec2 = spec1.copy(data = data1.copy(lastReadyMessage = data1.currentReadyMessage.asSome), info = info1)
      params.walletDb.persist(spec2.data.toPersistent, spec2.info.lastBalance, ewt.xPub.publicKey)
      context.system.eventStream.publish(spec2.data.currentReadyMessage)
      specs.update(ewt.xPub, spec2)
    }
  }

  override def receive: Receive = {
    case electrumWalletMessage: Any =>
      (specs.get(ewt.xPub).map(_.data), electrumWalletMessage, state) match {
        case Tuple3(Some(data), rawPersistentElectrumData: ByteVector, DISCONNECTED) if null == data.blockchain =>
          // We perform deserialization here because wallet data may get large and slow down UI thread as time goes by
          val persisted = Try(persistentDataCodec.decode(rawPersistentElectrumData.toBitVector).require.value).getOrElse(params.emptyPersistentData)
          val firstAccountKeys = for (idx <- math.max(persisted.accountKeysCount - 1000, 0) until persisted.accountKeysCount) yield derivePublicKey(ewt.accountMaster, idx)
          val firstChangeKeys = for (idx <- math.max(persisted.changeKeysCount - 1000, 0) until persisted.changeKeysCount) yield derivePublicKey(ewt.changeMaster, idx)

          val data1 = ElectrumData(Blockchain(ewt.chainHash, checkpoints = Vector.empty, headersMap = Map.empty, bestchain = Vector.empty),  MemoizedKeys(ewt, firstAccountKeys.toVector, firstChangeKeys.toVector),
            persisted.excludedOutPoints, persisted.status.withDefaultValue(new String), persisted.transactions, persisted.overriddenPendingTxids, persisted.history, persisted.proofs, pendingHistoryRequests = Set.empty,
            pendingHeadersRequests = Set.empty, pendingTransactionRequests = Set.empty, pendingTransactions = persisted.pendingTransactions)
          val spec1 = specs(ewt.xPub).copy(data = data1)
          specs.update(ewt.xPub, spec1)

        case (Some(data), blockchain1: Blockchain, DISCONNECTED) =>
          for (scriptHash <- data.keys.accountKeyMap.keys) client ! ElectrumClient.ScriptHashSubscription(scriptHash, self)
          for (scriptHash <- data.keys.changeKeyMap.keys) client ! ElectrumClient.ScriptHashSubscription(scriptHash, self)
          val data1 = data.copy(blockchain = blockchain1)
          persistAndNotify(data1)
          state = RUNNING

        case (Some(data), blockchain1: Blockchain, RUNNING) =>
          val data1 = data.copy(blockchain = blockchain1)
          data1.pendingMerkleResponses.foreach(self.!)
          persistAndNotify(data1)

        case (Some(data), ElectrumClient.ScriptHashSubscriptionResponse(scriptHash, status), RUNNING) if data.status.get(scriptHash).contains(status) =>
          val missing = data.history.getOrElse(scriptHash, Nil).map(item => item.txHash -> item.height).toMap -- data.transactions.keySet -- data.pendingTransactionRequests

          missing.foreach { case (txId, height) =>
            client ! GetTransaction(txid = txId)
            client ! GetMerkle(txId, height)
          }

          if (missing.nonEmpty) {
            // An optimization to not recalculate internal data values on each scriptHashResponse event
            val data1 = data.copy(pendingHistoryRequests = data.pendingTransactionRequests ++ missing.keySet)
            persistAndNotify(data1)
          }

        case (Some(data), ElectrumClient.ScriptHashSubscriptionResponse(scriptHash, status), RUNNING) if status.isEmpty =>
          // Generally we don't need to do anything here because most likely this is a fresh address where scriptHash is already empty
          // But if we did have something there while now there is nothing then an incoming transaction has likely been cancelled
          if (data.status contains scriptHash) client ! ElectrumClient.GetScriptHashHistory(scriptHash)

        case (Some(data), ElectrumClient.ScriptHashSubscriptionResponse(scriptHash, status), RUNNING) =>
          val data1 = data.copy(status = data.status.updated(scriptHash, status), pendingHistoryRequests = data.pendingHistoryRequests + scriptHash)
          client ! ElectrumClient.GetScriptHashHistory(scriptHash)
          persistAndNotify(data1)

        case (Some(data), ElectrumClient.GetScriptHashHistoryResponse(scriptHash, items), RUNNING) =>
          val pendingHeadersRequests1 = collection.mutable.HashSet.empty[GetHeaders]
          pendingHeadersRequests1 ++= data.pendingHeadersRequests

          def downloadHeadersIfMissing(height: Int): Unit = {
            if (data.blockchain.getHeader(height).orElse(params.headerDb getHeader height).isEmpty) {
              // we don't have this header because it is older than our checkpoints => request the entire chunk
              val request = GetHeaders(height / RETARGETING_PERIOD * RETARGETING_PERIOD, RETARGETING_PERIOD)
              if (pendingHeadersRequests1 contains request) return
              pendingHeadersRequests1.add(request)
              chainSync ! request
            }
          }

          def process(txid: ByteVector32, height: Int): Unit = {
            if (data.proofs.contains(txid) || height < 1) return
            downloadHeadersIfMissing(height)
            client ! GetMerkle(txid, height)
          }

          val pendingTransactionRequests1 = items.foldLeft(data.pendingTransactionRequests) {
            case (hashes, item) if !data.transactions.contains(item.txHash) && !data.pendingTransactionRequests.contains(item.txHash) =>
              client ! GetTransaction(item.txHash)
              process(item.txHash, item.height)
              hashes + item.txHash

            case (hashes, item) =>
              process(item.txHash, item.height)
              hashes
          }

          val shadowItems = for {
            existingItems <- data.history.get(scriptHash).toList
            item <- existingItems if !items.exists(_.txHash == item.txHash)
          } yield item.txHash

          val pendingTransactions1 = data.pendingTransactions.filterNot { pendingTx =>
            // We have a transaction that Electrum does not know about, remove it
            shadowItems.contains(pendingTx.txid)
          }

          val data1 = data.copy(history = data.history.updated(scriptHash, items),
            pendingHistoryRequests = data.pendingHistoryRequests - scriptHash,
            pendingTransactionRequests = pendingTransactionRequests1,
            pendingHeadersRequests = pendingHeadersRequests1.toSet,
            transactions = data.transactions -- shadowItems,
            pendingTransactions = pendingTransactions1)
          persistAndNotify(data1)

        case (Some(data), GetTransactionResponse(tx), RUNNING) =>
          val clearedExcludedOutPoints: List[OutPoint] = data.excludedOutPoints diff tx.txIn.map(_.outPoint)
          // Even though we have excluded some utxos in this wallet user may still spend them from other wallet, so clear excluded outpoints here
          val data1 = data.copy(pendingTransactionRequests = data.pendingTransactionRequests - tx.txid, excludedOutPoints = clearedExcludedOutPoints)

          val data2 = computeTxDelta(data :: Nil, tx) map { case TransactionDelta(_, received, sent) =>
            val addresses = tx.txOut.filter(data.isMine).map(_.publicKeyScript).flatMap(data.keys.publicScriptMap.get).map(ewt.textAddress).toList
            context.system.eventStream publish TransactionReceived(tx, data.depth(tx.txid), data.timestamp(tx.txid, params.headerDb), received, sent, addresses, ewt.xPub :: Nil)
            // Transactions arrive asynchronously, we may have valid txs wihtout parents, if that happens we wait until parents arrive and then retry delta check again
            for (stillPendingTx <- data.pendingTransactions) self ! GetTransactionResponse(stillPendingTx)
            data1.copy(transactions = data.transactions.updated(tx.txid, tx), pendingTransactions = Nil)
          } getOrElse data1.copy(pendingTransactions = data.pendingTransactions :+ tx)
          persistAndNotify(data2)

        case (Some(data), response: GetMerkleResponse, RUNNING) =>
          val request = GetHeaders(response.blockHeight / RETARGETING_PERIOD * RETARGETING_PERIOD, RETARGETING_PERIOD)
          data.blockchain.getHeader(response.blockHeight) orElse params.headerDb.getHeader(response.blockHeight) match {
            case Some(existingMerkleHeader) if existingMerkleHeader.hashMerkleRoot == response.root && data.isTxKnown(response.txid) =>
              val data1 = data.copy(proofs = data.proofs.updated(response.txid, response), pendingMerkleResponses = data.pendingMerkleResponses - response)
              persistAndNotify(data1.withOverridingTxids)

            case Some(existingHeader) if existingHeader.hashMerkleRoot == response.root =>
              // Do nothing but guard from this being matched further down the road

            case None if data.pendingHeadersRequests.contains(request) =>
              val data1 = data.copy(pendingMerkleResponses = data.pendingMerkleResponses + response)
              persistAndNotify(data1)

            case None =>
              chainSync ! request
              val data1 = data.copy(pendingHeadersRequests = data.pendingHeadersRequests + request)
              val data2 = data1.copy(pendingMerkleResponses = data1.pendingMerkleResponses + response)
              persistAndNotify(data2)

            case _ =>
              // Something is wrong with this client, better disconnect from it
              val data1 = data.copy(transactions = data.transactions - response.txid)
              persistAndNotify(data1)
              sender ! PoisonPill
          }

        case (Some(data), ElectrumClient.ElectrumDisconnected, _) =>
          persistAndNotify(data.reset)
          state = DISCONNECTED

        case (Some(data), SetExcludedOutPoints(excluded), _) =>
          val data1 = data.copy(excludedOutPoints = excluded)
          persistAndNotify(data1)

        case (Some(data), KEY_REFILL, _) if data.firstUnusedChangeKeys.size < MAX_RECEIVE_ADDRESSES =>
          val (memoizedKeys1, newKeyScriptHash) = data.keys.withNewChangeKey
          client ! ScriptHashSubscription(newKeyScriptHash, self)
          val data1 = data.copy(keys = memoizedKeys1)
          persistAndNotify(data1)

        case (Some(data), KEY_REFILL, _) if data.firstUnusedAccountKeys.size < MAX_RECEIVE_ADDRESSES =>
          val (memoizedKeys1, newKeyScriptHash) = data.keys.withNewAccountKey
          client ! ScriptHashSubscription(newKeyScriptHash, self)
          val data1 = data.copy(keys = memoizedKeys1)
          persistAndNotify(data1)

        case _ =>
          // Do nothing
      }
  }
}

case class AccountAndXPrivKey(xPriv: ExtendedPrivateKey, master: ExtendedPrivateKey)
case class TransactionDelta(spentUtxos: Seq[Utxo], received: Satoshi, sent: Satoshi)
case class Utxo(key: ExtendedPublicKey, item: ElectrumClient.UnspentItem, ewt: ElectrumWalletType)

case class WalletSpec(info: CompleteBtcWalletInfo, data: ElectrumData, walletRef: ActorRef) {
  def withNewLabel(label: String): WalletSpec = WalletSpec(info.copy(label = label), data, walletRef)
  def usable: Boolean = data.keys.ewt.secrets.nonEmpty || info.core.masterFingerprint.nonEmpty
  def spendable: Boolean = info.lastBalance > 0L.sat
}

case class WalletParameters(headerDb: SQLiteData, walletDb: SQLiteBtcWallet, txDb: SQLiteBtcTx, dustLimit: Satoshi = 546L.sat) {
  val emptyPersistentData: PersistentData = PersistentData(ElectrumWallet.MAX_RECEIVE_ADDRESSES, ElectrumWallet.MAX_RECEIVE_ADDRESSES)
  val emptyPersistentDataBytes: ByteVector = persistentDataCodec.encode(emptyPersistentData).require.toByteVector
}

case class MemoizedKeys(ewt: ElectrumWalletType, accountKeys: Vector[ExtendedPublicKey] = Vector.empty, changeKeys: Vector[ExtendedPublicKey] = Vector.empty) {
  lazy val publicScriptAccountMap: Map[ByteVector, ExtendedPublicKey] = accountKeys.map(key => ewt.writePublicKeyScriptHash(key.publicKey) -> key).toMap
  lazy val publicScriptChangeMap: Map[ByteVector, ExtendedPublicKey] = changeKeys.map(key => ewt.writePublicKeyScriptHash(key.publicKey) -> key).toMap
  lazy val publicScriptMap: Map[ByteVector, ExtendedPublicKey] = publicScriptAccountMap ++ publicScriptChangeMap

  lazy val accountKeyMap: Map[ByteVector32, ExtendedPublicKey] = for (serialized \ key <- publicScriptAccountMap) yield (computeScriptHash(serialized), key)
  lazy val changeKeyMap: Map[ByteVector32, ExtendedPublicKey] = for (serialized \ key <- publicScriptChangeMap) yield (computeScriptHash(serialized), key)

  def withNewAccountKey: (MemoizedKeys, ByteVector32) = {
    val newKey = derivePublicKey(ewt.accountMaster, accountKeys.last.path.lastChildNumber + 1)
    val newKeyScriptHash = computeScriptHash(ewt writePublicKeyScriptHash newKey.publicKey)
    (copy(accountKeys = accountKeys :+ newKey), newKeyScriptHash)
  }

  def withNewChangeKey: (MemoizedKeys, ByteVector32) = {
    val newKey = derivePublicKey(ewt.changeMaster, changeKeys.last.path.lastChildNumber + 1)
    val newKeyScriptHash = computeScriptHash(ewt writePublicKeyScriptHash newKey.publicKey)
    (copy(changeKeys = changeKeys :+ newKey), newKeyScriptHash)
  }
}

case class ElectrumData(blockchain: Blockchain, keys: MemoizedKeys, excludedOutPoints: List[OutPoint] = Nil, status: Map[ByteVector32, String] = Map.empty, transactions: Map[ByteVector32, Transaction] = Map.empty,
                        overriddenPendingTxids: Map[ByteVector32, ByteVector32] = Map.empty, history: Map[ByteVector32, ElectrumWallet.TxHistoryItemList] = Map.empty, proofs: Map[ByteVector32, GetMerkleResponse] = Map.empty,
                        pendingHistoryRequests: Set[ByteVector32] = Set.empty, pendingTransactionRequests: Set[ByteVector32] = Set.empty, pendingHeadersRequests: Set[GetHeaders] = Set.empty, pendingTransactions: List[Transaction] = Nil,
                        pendingMerkleResponses: Set[GetMerkleResponse] = Set.empty, lastReadyMessage: Option[ElectrumWallet.WalletReady] = None) {

  lazy val currentReadyMessage: ElectrumWallet.WalletReady = ElectrumWallet.WalletReady(balance, blockchain.tip.height, proofs.hashCode + transactions.hashCode, keys.ewt.xPub, unExcludedUtxos, excludedOutPoints)

  lazy val firstUnusedAccountKeys: immutable.Iterable[ExtendedPublicKey] = keys.accountKeyMap.collect {
    case (scriptHash, pubAccountKey) if status(scriptHash) == new String => pubAccountKey
  }

  lazy val firstUnusedChangeKeys: immutable.Iterable[ExtendedPublicKey] = {
    val usedChangeNumbers = transactions.values.flatMap(_.txOut).map(_.publicKeyScript).flatMap(keys.publicScriptChangeMap.get).map(_.path.lastChildNumber).toSet
    keys.changeKeys.collect { case unusedChangeKey if !usedChangeNumbers.contains(unusedChangeKey.path.lastChildNumber) => unusedChangeKey }
  }

  lazy val unExcludedUtxos: Seq[Utxo] = {
    history.toSeq flatMap { case (scriptHash, historyItems) =>
      keys.accountKeyMap.get(scriptHash) orElse keys.changeKeyMap.get(scriptHash) map { key =>
        // We definitely have a private key generated for corresponding scriptHash here

        val unspents = for {
          item <- historyItems
          tx <- transactions.get(item.txHash).toList
          if !overriddenPendingTxids.contains(tx.txid)
          (txOut, index) <- tx.txOut.zipWithIndex if computeScriptHash(txOut.publicKeyScript) == scriptHash
        } yield Utxo(key, UnspentItem(item.txHash, index, txOut.amount.toLong, item.height), keys.ewt)

        // Find all out points which spend from this script hash and make sure unspents do not contain them
        val outPoints = historyItems.map(_.txHash).flatMap(transactions.get).flatMap(_.txIn).map(_.outPoint).toSet
        unspents.filterNot(utxo => outPoints contains utxo.item.outPoint)
      } getOrElse Nil
    }
  }

  lazy val utxos: Seq[Utxo] = unExcludedUtxos.filterNot(utxo => excludedOutPoints contains utxo.item.outPoint)

  lazy val balance: Satoshi = utxos.foldLeft(0L.sat) { case (accumulator, utxo) => accumulator + utxo.item.value.sat }

  // Remove status for each script hash for which we have pending requests, this will make us query script hash history for these script hashes again when we reconnect
  def reset: ElectrumData = copy(status = status -- pendingHistoryRequests, pendingHistoryRequests = Set.empty, pendingTransactionRequests = Set.empty, pendingHeadersRequests = Set.empty, lastReadyMessage = None)

  def toPersistent: PersistentData = PersistentData(keys.accountKeys.length, keys.changeKeys.length, status, transactions, overriddenPendingTxids, history, proofs, pendingTransactions, excludedOutPoints)

  def isTxKnown(txid: ByteVector32): Boolean = transactions.contains(txid) || pendingTransactionRequests.contains(txid) || pendingTransactions.exists(_.txid == txid)

  def extPubKeyFromInput(txIn: TxIn): Option[ExtendedPublicKey] = keys.ewt.extractPubKeySpentFrom(txIn).map(keys.ewt.writePublicKeyScriptHash).flatMap(keys.publicScriptMap.get)

  def isMine(txOut: TxOut): Boolean = keys.publicScriptMap.contains(txOut.publicKeyScript)

  def changePubKey: ExtendedPublicKey = firstUnusedChangeKeys.headOption.getOrElse(keys.changeKeys.head)

  def timestamp(txid: ByteVector32, headerDb: SQLiteData): Long = {
    val blockHeight = proofs.get(txid).map(_.blockHeight).getOrElse(default = 0)
    val stampOpt = blockchain.getHeader(blockHeight) orElse headerDb.getHeader(blockHeight)
    stampOpt.map(_.time * 1000L).getOrElse(System.currentTimeMillis)
  }

  def depth(txid: ByteVector32): Int = proofs.get(txid).map(_.blockHeight).map(computeDepth).getOrElse(0)
  def computeDepth(txHeight: Int): Int = if (txHeight <= 0L) 0 else blockchain.height - txHeight + 1

  def withOverridingTxids: ElectrumData = {
    def changeKeyDepth(tx: Transaction) = if (proofs contains tx.txid) Long.MaxValue else tx.txOut.map(_.publicKeyScript).flatMap(keys.publicScriptChangeMap.get).map(_.path.lastChildNumber).headOption.getOrElse(Long.MinValue)
    def computeOverride(txs: Iterable[Transaction] = Nil) = Stream.continually(txs maxBy changeKeyDepth) zip txs collect { case (latterTx, formerTx) if latterTx != formerTx => formerTx.txid -> latterTx.txid }
    val overrides = transactions.values.map(Stream continually _).flatMap(txStream => txStream.head.txIn zip txStream).groupBy(_._1.outPoint).values.map(_.secondItems).flatMap(computeOverride)
    copy(overriddenPendingTxids = overrides.toMap)
  }
}

case class PersistentData(accountKeysCount: Int, changeKeysCount: Int, status: Map[ByteVector32, String] = Map.empty,
                          transactions: Map[ByteVector32, Transaction] = Map.empty, overriddenPendingTxids: Map[ByteVector32, ByteVector32] = Map.empty,
                          history: Map[ByteVector32, ElectrumWallet.TxHistoryItemList] = Map.empty, proofs: Map[ByteVector32, GetMerkleResponse] = Map.empty,
                          pendingTransactions: List[Transaction] = Nil, excludedOutPoints: List[OutPoint] = Nil)
