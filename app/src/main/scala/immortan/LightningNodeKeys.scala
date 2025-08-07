package immortan

import fr.acinq.bitcoin.Crypto.PrivateKey
import fr.acinq.bitcoin.DeterministicWallet._
import immortan.crypto.Tools.{Bytes, StringList}
import scodec.bits.ByteVector


object LightningNodeKeys {
  def fromSeed(seed: Bytes): LightningNodeKeys = {
    val master = generate(ByteVector view seed)
    fromMaster(master)
  }

  def fromMaster(master: ExtendedPrivateKey): LightningNodeKeys = {
    val extendedNodeKey: ExtendedPrivateKey = derivePrivateKey(master, hardened(46L) :: hardened(0L) :: Nil)
    val hashingKey: PrivateKey = derivePrivateKey(master, hardened(138L) :: 0L :: Nil).privateKey
    LightningNodeKeys(master, extendedNodeKey, hashingKey)
  }
}

case class LightningNodeKeys(master: ExtendedPrivateKey, extendedNodeKey: ExtendedPrivateKey, hashingKey: PrivateKey)
case class WalletSecret(keys: LightningNodeKeys, mnemonic: StringList, seed: ByteVector)