package fr.acinq.eclair.blockchain.electrum

import fr.acinq.bitcoin.Crypto.PublicKey
import fr.acinq.bitcoin.DeterministicWallet._
import fr.acinq.bitcoin._
import fr.acinq.eclair.blockchain.EclairWallet
import immortan.crypto.Tools.Any2Some
import scodec.bits.ByteVector

import scala.util.Try


object ElectrumWalletType {
  def makeSigningType(tag: String, master: ExtendedPrivateKey, chainHash: ByteVector32): ElectrumWalletType = tag match {
    case EclairWallet.BIP32 => makeSigningType(tag, xPriv32(master, chainHash), chainHash)
    case EclairWallet.BIP44 => makeSigningType(tag, xPriv44(master, chainHash), chainHash)
    case EclairWallet.BIP49 => makeSigningType(tag, xPriv49(master, chainHash), chainHash)
    case EclairWallet.BIP84 => makeSigningType(tag, xPriv84(master, chainHash), chainHash)
    case _ => throw new RuntimeException
  }

  def makeSigningType(tag: String, secrets: AccountAndXPrivKey, chainHash: ByteVector32): ElectrumWalletType = tag match {
    case EclairWallet.BIP32 => new ElectrumWallet32(secrets.asSome, publicKey(secrets.xPriv), chainHash)
    case EclairWallet.BIP44 => new ElectrumWallet44(secrets.asSome, publicKey(secrets.xPriv), chainHash)
    case EclairWallet.BIP49 => new ElectrumWallet49(secrets.asSome, publicKey(secrets.xPriv), chainHash)
    case EclairWallet.BIP84 => new ElectrumWallet84(secrets.asSome, publicKey(secrets.xPriv), chainHash)
    case _ => throw new RuntimeException
  }

  def xPriv32(master: ExtendedPrivateKey, chainHash: ByteVector32): AccountAndXPrivKey = chainHash match {
    case Block.RegtestGenesisBlock.hash | Block.TestnetGenesisBlock.hash | Block.Testnet4GenesisBlock.hash => AccountAndXPrivKey(derivePrivateKey(master, hardened(1L) :: 0L :: Nil), master)
    case Block.LivenetGenesisBlock.hash => AccountAndXPrivKey(derivePrivateKey(master, hardened(0L) :: 0L :: Nil), master)
    case _ => throw new RuntimeException
  }

  def xPriv44(master: ExtendedPrivateKey, chainHash: ByteVector32): AccountAndXPrivKey = chainHash match {
    case Block.RegtestGenesisBlock.hash | Block.TestnetGenesisBlock.hash | Block.Testnet4GenesisBlock.hash => AccountAndXPrivKey(derivePrivateKey(master, hardened(44L) :: hardened(1L) :: hardened(0L) :: Nil), master)
    case Block.LivenetGenesisBlock.hash => AccountAndXPrivKey(derivePrivateKey(master, hardened(44L) :: hardened(0L) :: hardened(0L) :: Nil), master)
    case _ => throw new RuntimeException
  }

  def xPriv49(master: ExtendedPrivateKey, chainHash: ByteVector32): AccountAndXPrivKey = chainHash match {
    case Block.RegtestGenesisBlock.hash | Block.TestnetGenesisBlock.hash | Block.Testnet4GenesisBlock.hash => AccountAndXPrivKey(derivePrivateKey(master, hardened(49L) :: hardened(1L) :: hardened(0L) :: Nil), master)
    case Block.LivenetGenesisBlock.hash => AccountAndXPrivKey(derivePrivateKey(master, hardened(49L) :: hardened(0L) :: hardened(0L) :: Nil), master)
    case _ => throw new RuntimeException
  }

  def xPriv84(master: ExtendedPrivateKey, chainHash: ByteVector32): AccountAndXPrivKey = chainHash match {
    case Block.RegtestGenesisBlock.hash | Block.TestnetGenesisBlock.hash | Block.Testnet4GenesisBlock.hash => AccountAndXPrivKey(derivePrivateKey(master, hardened(84L) :: hardened(1L) :: hardened(0L) :: Nil), master)
    case Block.LivenetGenesisBlock.hash => AccountAndXPrivKey(derivePrivateKey(master, hardened(84L) :: hardened(0L) :: hardened(0L) :: Nil), master)
    case _ => throw new RuntimeException
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

  def signTransaction(usableUtxos: Seq[Utxo], tx: Transaction): Transaction

  def setUtxosWithDummySig(usableUtxos: Seq[Utxo], tx: Transaction, sequenceFlag: Long): Transaction

  def writePublicKeyScriptHash(key: PublicKey): ByteVector = {
    val scriptProgram = computePublicKeyScript(key)
    Script.write(scriptProgram)
  }
}

class ElectrumWallet44(val secrets: Option[AccountAndXPrivKey], val xPub: ExtendedPublicKey, val chainHash: ByteVector32) extends ElectrumWalletType {

  override def textAddress(key: ExtendedPublicKey): String = computeP2PkhAddress(key.publicKey, chainHash)

  override def computePublicKeyScript(key: PublicKey): Seq[ScriptElt] = Script.pay2pkh(key)

  override def extractPubKeySpentFrom(txIn: TxIn): Option[PublicKey] = Try {
    val _ :: OP_PUSHDATA(data, _) :: Nil = Script.parse(txIn.signatureScript)
    PublicKey(data)
  }.toOption

  override def setUtxosWithDummySig(usableUtxos: Seq[Utxo], tx: Transaction, sequenceFlag: Long): Transaction = {
    val txIn1 = for {
      utxo <- usableUtxos
      dummySig = ByteVector.fill(71)(1)
      sigScript = Script.write(OP_PUSHDATA(dummySig) :: OP_PUSHDATA(utxo.key.publicKey) :: Nil)
    } yield TxIn(utxo.item.outPoint, sigScript, sequenceFlag)
    tx.copy(txIn = txIn1)
  }

  override def signTransaction(usableUtxos: Seq[Utxo], tx: Transaction): Transaction = {
    val txIn1 = for {
      (txIn, idx) <- tx.txIn.zipWithIndex
      utxo <- usableUtxos.find(_.item.outPoint == txIn.outPoint)
      previousOutputScript = Script.pay2pkh(pubKey = utxo.key.publicKey)
      privateKey = derivePrivateKey(secrets.map(_.master).getOrElse(fr.acinq.eclair.dummyExtPrivKey), utxo.key.path).privateKey
      sig = Transaction.signInput(tx, idx, previousOutputScript, SIGHASH_ALL, utxo.item.value.sat, SigVersion.SIGVERSION_BASE, privateKey)
      sigScript = Script.write(OP_PUSHDATA(sig) :: OP_PUSHDATA(utxo.key.publicKey) :: Nil)
    } yield txIn.copy(signatureScript = sigScript)
    tx.copy(txIn = txIn1)
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

  override def setUtxosWithDummySig(usableUtxos: Seq[Utxo], tx: Transaction, sequenceFlag: Long): Transaction = {
    val txIn1 = for {
      utxo <- usableUtxos
      pubKeyScript = Script.write(Script pay2wpkh utxo.key.publicKey)
      witness = ScriptWitness(ByteVector.fill(71)(1) :: utxo.key.publicKey.value :: Nil)
    } yield TxIn(utxo.item.outPoint, Script.write(OP_PUSHDATA(pubKeyScript) :: Nil), sequenceFlag, witness)
    tx.copy(txIn = txIn1)
  }

  override def signTransaction(usableUtxos: Seq[Utxo], tx: Transaction): Transaction = {
    val txIn1 = for {
      (txIn, idx) <- tx.txIn.zipWithIndex
      utxo <- usableUtxos.find(_.item.outPoint == txIn.outPoint)
      pubKeyScript = Script.write(Script pay2wpkh utxo.key.publicKey)
      privateKey = derivePrivateKey(secrets.map(_.master).getOrElse(fr.acinq.eclair.dummyExtPrivKey), utxo.key.path).privateKey
      sig = Transaction.signInput(tx, idx, Script.pay2pkh(utxo.key.publicKey), SIGHASH_ALL, utxo.item.value.sat, SigVersion.SIGVERSION_WITNESS_V0, privateKey)
    } yield txIn.copy(signatureScript = Script.write(OP_PUSHDATA(pubKeyScript) :: Nil), witness = ScriptWitness(sig :: utxo.key.publicKey.value :: Nil))
    tx.copy(txIn = txIn1)
  }
}

class ElectrumWallet84(val secrets: Option[AccountAndXPrivKey], val xPub: ExtendedPublicKey, val chainHash: ByteVector32) extends ElectrumWalletType {

  override def textAddress(key: ExtendedPublicKey): String = computeBIP84Address(key.publicKey, chainHash)

  override def extractPubKeySpentFrom(txIn: TxIn): Option[PublicKey] = Try(txIn.witness.stack.last).map(PublicKey.apply).toOption

  override def computePublicKeyScript(key: PublicKey): Seq[ScriptElt] = Script.pay2wpkh(key)

  override def setUtxosWithDummySig(usableUtxos: Seq[Utxo], tx: Transaction, sequenceFlag: Long): Transaction = {
    val txIn1 = for {
      utxo <- usableUtxos
      witness = ScriptWitness(ByteVector.fill(71)(1) :: utxo.key.publicKey.value :: Nil)
    } yield TxIn(utxo.item.outPoint, signatureScript = ByteVector.empty, sequenceFlag, witness)
    tx.copy(txIn = txIn1)
  }

  override def signTransaction(usableUtxos: Seq[Utxo], tx: Transaction): Transaction = {
    val txIn1 = for {
      (txIn, idx) <- tx.txIn.zipWithIndex
      utxo <- usableUtxos.find(_.item.outPoint == txIn.outPoint)
      privateKey = derivePrivateKey(secrets.map(_.master).getOrElse(fr.acinq.eclair.dummyExtPrivKey), utxo.key.path).privateKey
      sig = Transaction.signInput(tx, idx, Script.pay2pkh(utxo.key.publicKey), SIGHASH_ALL, utxo.item.value.sat, SigVersion.SIGVERSION_WITNESS_V0, privateKey)
    } yield txIn.copy(witness = ScriptWitness(sig :: utxo.key.publicKey.value :: Nil), signatureScript = ByteVector.empty)
    tx.copy(txIn = txIn1)
  }
}
