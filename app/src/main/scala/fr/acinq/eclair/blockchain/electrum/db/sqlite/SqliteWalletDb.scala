package fr.acinq.eclair.blockchain.electrum.db.sqlite

import fr.acinq.bitcoin.{ByteVector32, Transaction}
import fr.acinq.eclair.blockchain.electrum.ElectrumClient.{GetMerkleResponse, TransactionHistoryItem}
import fr.acinq.eclair.blockchain.electrum.{ElectrumClient, ElectrumWallet, PersistentData}
import fr.acinq.eclair.wire.CommonCodecs._
import scodec.Codec
import scodec.bits.BitVector
import scodec.codecs._

object SqliteWalletDb {
  val proofCodec = {
    (bytes32 withContext "txid") ::
      (listOfN(uint16, bytes32) withContext "merkle") ::
      (uint24 withContext "blockHeight") ::
      (uint24 withContext "pos")
  }.as[GetMerkleResponse]

  type OverriddenPendingTxids = Map[ByteVector32, ByteVector32]

  val overrideCodec: Codec[OverriddenPendingTxids] = Codec[OverriddenPendingTxids](
    (runtimeMap: OverriddenPendingTxids) => listOfN(uint16, bytes32 ~ bytes32).encode(runtimeMap.toList),
    (wire: BitVector) => listOfN(uint16, bytes32 ~ bytes32).decode(wire).map(_.map(_.toMap))
  )

  type Status = Map[ByteVector32, String]

  val statusCodec: Codec[Status] = Codec[Status](
    (runtimeMap: Status) => listOfN(uint16, bytes32 ~ cstring).encode(runtimeMap.toList),
    (wire: BitVector) => listOfN(uint16, bytes32 ~ cstring).decode(wire).map(_.map(_.toMap))
  )

  type Transactions = Map[ByteVector32, Transaction]

  val transactionsCodec: Codec[Transactions] = Codec[Transactions](
    (runtimeMap: Transactions) => listOfN(uint16, bytes32 ~ txCodec).encode(runtimeMap.toList),
    (wire: BitVector) => listOfN(uint16, bytes32 ~ txCodec).decode(wire).map(_.map(_.toMap))
  )

  val transactionHistoryItemCodec = {
    (int32 withContext "height") ::
      (bytes32 withContext "txHash")
  }.as[ElectrumClient.TransactionHistoryItem]

  val seqOfTransactionHistoryItemCodec = listOfN[TransactionHistoryItem](uint16, transactionHistoryItemCodec)

  type History = Map[ByteVector32, ElectrumWallet.TxHistoryItemList]

  val historyCodec: Codec[History] = Codec[History](
    (runtimeMap: History) => listOfN(uint16, bytes32 ~ seqOfTransactionHistoryItemCodec).encode(runtimeMap.toList),
    (wire: BitVector) => listOfN(uint16, bytes32 ~ seqOfTransactionHistoryItemCodec).decode(wire).map(_.map(_.toMap))
  )

  type Proofs = Map[ByteVector32, GetMerkleResponse]

  val proofsCodec: Codec[Proofs] = Codec[Proofs](
    (runtimeMap: Proofs) => listOfN(uint16, bytes32 ~ proofCodec).encode(runtimeMap.toList),
    (wire: BitVector) => listOfN(uint16, bytes32 ~ proofCodec).decode(wire).map(_.map(_.toMap))
  )

  val persistentDataCodec: Codec[PersistentData] = {
      (int32 withContext "accountKeysCount") ::
      (int32 withContext "changeKeysCount") ::
      (statusCodec withContext "status") ::
      (transactionsCodec withContext "transactions") ::
      (overrideCodec withContext "overriddenPendingTxids") ::
      (historyCodec withContext "history") ::
      (proofsCodec withContext "proofs") ::
      (listOfN(uint16, txCodec) withContext "pendingTransactions") ::
      (listOfN(uint16, outPointCodec) withContext "excludedOutpoints")
  }.as[PersistentData]
}
