package fr.acinq.eclair.blockchain.electrum

import fr.acinq.bitcoin.Crypto.PublicKey
import fr.acinq.bitcoin.DeterministicWallet._
import fr.acinq.bitcoin._
import immortan.crypto.Tools._
import scodec.bits.ByteVector

import scala.util.Try


object ElectrumWalletType {
  def makeSigningType(tag: String, master: ExtendedPrivateKey, hash: ByteVector32, ord: Long): ElectrumWalletType = tag match {
    case ElectrumWallet.BIP32 => makeSigningType(tag, secrets = xPriv32(master, hash, ord), hash)
    case ElectrumWallet.BIP44 => makeSigningType(tag, secrets = xPriv44(master, hash, ord), hash)
    case ElectrumWallet.BIP49 => makeSigningType(tag, secrets = xPriv49(master, hash, ord), hash)
    case ElectrumWallet.BIP84 => makeSigningType(tag, secrets = xPriv84(master, hash, ord), hash)
    case _ => throw new RuntimeException
  }

  def makeSigningType(tag: String, secrets: AccountAndXPrivKey, hash: ByteVector32): ElectrumWalletType = tag match {
    case ElectrumWallet.BIP32 => new ElectrumWallet32(secrets.asSome, publicKey(secrets.xPriv), hash)
    case ElectrumWallet.BIP44 => new ElectrumWallet44(secrets.asSome, publicKey(secrets.xPriv), hash)
    case ElectrumWallet.BIP49 => new ElectrumWallet49(secrets.asSome, publicKey(secrets.xPriv), hash)
    case ElectrumWallet.BIP84 => new ElectrumWallet84(secrets.asSome, publicKey(secrets.xPriv), hash)
    case _ => throw new RuntimeException
  }

  def xPriv32(master: ExtendedPrivateKey, hash: ByteVector32, ord: Long): AccountAndXPrivKey = hash match {
    case Block.RegtestGenesisBlock.hash => AccountAndXPrivKey(derivePrivateKey(master, hardened(1L) :: ord :: Nil), master)
    case Block.TestnetGenesisBlock.hash => AccountAndXPrivKey(derivePrivateKey(master, hardened(1L) :: ord :: Nil), master)
    case Block.LivenetGenesisBlock.hash => AccountAndXPrivKey(derivePrivateKey(master, hardened(0L) :: ord :: Nil), master)
    case _ => throw new RuntimeException
  }

  def xPriv44(master: ExtendedPrivateKey, hash: ByteVector32, ord: Long): AccountAndXPrivKey = hash match {
    case Block.RegtestGenesisBlock.hash => AccountAndXPrivKey(derivePrivateKey(master, hardened(44L) :: hardened(1L) :: hardened(ord) :: Nil), master)
    case Block.TestnetGenesisBlock.hash => AccountAndXPrivKey(derivePrivateKey(master, hardened(44L) :: hardened(1L) :: hardened(ord) :: Nil), master)
    case Block.LivenetGenesisBlock.hash => AccountAndXPrivKey(derivePrivateKey(master, hardened(44L) :: hardened(0L) :: hardened(ord) :: Nil), master)
    case _ => throw new RuntimeException
  }

  def xPriv49(master: ExtendedPrivateKey, hash: ByteVector32, ord: Long): AccountAndXPrivKey = hash match {
    case Block.RegtestGenesisBlock.hash => AccountAndXPrivKey(derivePrivateKey(master, hardened(49L) :: hardened(1L) :: hardened(ord) :: Nil), master)
    case Block.TestnetGenesisBlock.hash => AccountAndXPrivKey(derivePrivateKey(master, hardened(49L) :: hardened(1L) :: hardened(ord) :: Nil), master)
    case Block.LivenetGenesisBlock.hash => AccountAndXPrivKey(derivePrivateKey(master, hardened(49L) :: hardened(0L) :: hardened(ord) :: Nil), master)
    case _ => throw new RuntimeException
  }

  def xPriv84(master: ExtendedPrivateKey, hash: ByteVector32, ord: Long): AccountAndXPrivKey = hash match {
    case Block.RegtestGenesisBlock.hash => AccountAndXPrivKey(derivePrivateKey(master, hardened(84L) :: hardened(1L) :: hardened(ord) :: Nil), master)
    case Block.TestnetGenesisBlock.hash => AccountAndXPrivKey(derivePrivateKey(master, hardened(84L) :: hardened(1L) :: hardened(ord) :: Nil), master)
    case Block.LivenetGenesisBlock.hash => AccountAndXPrivKey(derivePrivateKey(master, hardened(84L) :: hardened(0L) :: hardened(ord) :: Nil), master)
    case _ => throw new RuntimeException
  }

  def signTransaction(usableUtxos: Seq[Utxo], tx: Transaction): (Seq[ElectrumWalletType], Transaction) = {
    val (wallets, inputs) = tx.txIn.zipWithIndex.map { case (unsignedTxInput, index) =>
      val utxo = usableUtxos.find(_.item.outPoint == unsignedTxInput.outPoint).get
      utxo.ewt -> utxo.ewt.signInput(utxo, tx, unsignedTxInput, index)
    }.unzip

    // All inputs must be matched with utxos
    wallets -> tx.copy(txIn = inputs)
  }

  def dummySignTransaction(usableUtxos: Seq[Utxo], tx: Transaction, sequenceFlag: Long): Transaction = {
    val viabletxInputs = for (utxo <- usableUtxos) yield utxo.ewt.dummySignInput(utxo, sequenceFlag)
    tx.copy(txIn = viabletxInputs)
  }
}

abstract class ElectrumWalletType {
  val secrets: Option[AccountAndXPrivKey]

  val xPub: ExtendedPublicKey

  val chainHash: ByteVector32

  val changeMaster: ExtendedPublicKey = derivePublicKey(xPub, 1L :: Nil)

  val accountMaster: ExtendedPublicKey = derivePublicKey(xPub, 0L :: Nil)

  def textAddress(key: ExtendedPublicKey): String

  def computePublicKeyScript(key: PublicKey): Seq[ScriptElt]

  def extractPubKeySpentFrom(txIn: TxIn): Option[PublicKey]

  def signInput(utxo: Utxo, tx: Transaction, input: TxIn, index: Int): TxIn

  def dummySignInput(utxo: Utxo, sequenceFlag: Long): TxIn

  def writePublicKeyScriptHash(key: PublicKey): ByteVector = {
    val scriptProgram = computePublicKeyScript(key)
    Script.write(scriptProgram)
  }

  def extPrivKeyFromPub(extPubKey: ExtendedPublicKey): ExtendedPrivateKey = {
    val key = for (secret <- secrets) yield derivePrivateKey(secret.master, extPubKey.path)
    key getOrElse fr.acinq.eclair.dummyExtPrivKey
  }
}

class ElectrumWallet44(val secrets: Option[AccountAndXPrivKey], val xPub: ExtendedPublicKey, val chainHash: ByteVector32) extends ElectrumWalletType {

  override def textAddress(key: ExtendedPublicKey): String = computeP2PkhAddress(key.publicKey, chainHash)

  override def computePublicKeyScript(key: PublicKey): Seq[ScriptElt] = Script.pay2pkh(key)

  override def extractPubKeySpentFrom(txIn: TxIn): Option[PublicKey] = Try {
    val _ :: OP_PUSHDATA(data, _) :: Nil = Script.parse(txIn.signatureScript)
    PublicKey(data)
  }.toOption

  override def dummySignInput(utxo: Utxo, sequenceFlag: Long): TxIn = {
    val sigScript = OP_PUSHDATA(ByteVector.fill(71)(1).compact) :: OP_PUSHDATA(utxo.key.publicKey) :: Nil
    TxIn(utxo.item.outPoint, Script.write(sigScript), sequenceFlag)
  }

  override def signInput(utxo: Utxo, tx: Transaction, input: TxIn, index: Int): TxIn = {
    val sig = Transaction.signInput(tx, index, Script.pay2pkh(utxo.key.publicKey), SIGHASH_ALL, utxo.item.value.sat, SigVersion.SIGVERSION_BASE, extPrivKeyFromPub(utxo.key).privateKey)
    val sigScript = Script.write(OP_PUSHDATA(sig) :: OP_PUSHDATA(utxo.key.publicKey) :: Nil)
    input.copy(signatureScript = sigScript)
  }
}

class ElectrumWallet32(override val secrets: Option[AccountAndXPrivKey], override val xPub: ExtendedPublicKey, override val chainHash: ByteVector32) extends ElectrumWallet44(secrets, xPub, chainHash) {

  override val changeMaster: ExtendedPublicKey = {
    val bip32ChangePath = KeyPath(hardened(0L) :: 1L :: Nil)
    val priv = derivePrivateKey(secrets.get.master, bip32ChangePath)
    publicKey(priv)
  }

  override val accountMaster: ExtendedPublicKey = xPub
}

class ElectrumWallet49(val secrets: Option[AccountAndXPrivKey], val xPub: ExtendedPublicKey, val chainHash: ByteVector32) extends ElectrumWalletType {

  override def textAddress(key: ExtendedPublicKey): String = computeBIP49Address(key.publicKey, chainHash)

  override def computePublicKeyScript(key: PublicKey): Seq[ScriptElt] = Script.pay2sh(Script pay2wpkh key)

  override def extractPubKeySpentFrom(txIn: TxIn): Option[PublicKey] = {
    Try {
      require(txIn.witness.stack.size == 2)
      val publicKey = PublicKey(txIn.witness.stack.tail.head)
      val OP_PUSHDATA(script, _) :: Nil = Script.parse(txIn.signatureScript)
      require(Script.write(Script pay2wpkh publicKey) == script)
      publicKey
    }.toOption
  }

  override def dummySignInput(utxo: Utxo, sequenceFlag: Long): TxIn = {
    val pubKeyScript = OP_PUSHDATA(Script.write(script = Script pay2wpkh utxo.key.publicKey).compact) :: Nil
    val witness = ScriptWitness(ByteVector.fill(71)(1) :: utxo.key.publicKey.value :: Nil)
    TxIn(utxo.item.outPoint, Script.write(pubKeyScript), sequenceFlag, witness)
  }

  override def signInput(utxo: Utxo, tx: Transaction, input: TxIn, index: Int): TxIn = {
    val sig = Transaction.signInput(tx, index, Script.pay2pkh(utxo.key.publicKey), SIGHASH_ALL, utxo.item.value.sat, SigVersion.SIGVERSION_WITNESS_V0, extPrivKeyFromPub(utxo.key).privateKey)
    val sigScript = Script.write(script = OP_PUSHDATA(Script.write(script = Script pay2wpkh utxo.key.publicKey).compact) :: Nil)
    input.copy(witness = ScriptWitness(sig :: utxo.key.publicKey.value :: Nil), signatureScript = sigScript)
  }
}

class ElectrumWallet84(val secrets: Option[AccountAndXPrivKey], val xPub: ExtendedPublicKey, val chainHash: ByteVector32) extends ElectrumWalletType {

  override def textAddress(key: ExtendedPublicKey): String = computeBIP84Address(key.publicKey, chainHash)

  override def extractPubKeySpentFrom(txIn: TxIn): Option[PublicKey] = Try(txIn.witness.stack.last).map(PublicKey.apply).toOption

  override def computePublicKeyScript(key: PublicKey): Seq[ScriptElt] = Script.pay2wpkh(key)

  override def dummySignInput(utxo: Utxo, sequenceFlag: Long): TxIn = {
    val witness = ScriptWitness(ByteVector.fill(71)(1) :: utxo.key.publicKey.value :: Nil)
    TxIn(utxo.item.outPoint, signatureScript = ByteVector.empty, sequenceFlag, witness)
  }

  override def signInput(utxo: Utxo, tx: Transaction, input: TxIn, index: Int): TxIn = {
    val sig = Transaction.signInput(tx, index, Script.pay2pkh(utxo.key.publicKey), SIGHASH_ALL, utxo.item.value.sat, SigVersion.SIGVERSION_WITNESS_V0, extPrivKeyFromPub(utxo.key).privateKey)
    input.copy(witness = ScriptWitness(sig :: utxo.key.publicKey.value :: Nil), signatureScript = ByteVector.empty)
  }
}
