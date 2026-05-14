package trading.tacticaladvantage

import trading.tacticaladvantage.Tools.{Bytes, StringList}
import fr.acinq.bitcoin.DeterministicWallet._
import fr.acinq.eclair.wire.CommonCodecs._
import scodec.Codec
import scodec.bits.ByteVector
import scodec.codecs._


object MasterKeys {
  def fromSeed(seed: Bytes): MasterKeys = {
    val bitcoinMaster = generate(ByteVector.view(seed), "Bitcoin seed")
    val sideChainsMaster = generate(ByteVector.view(seed), "Sidechains superseed")
    MasterKeys(bitcoinMaster, sideChainsMaster)
  }

  val masterKeysCodec: Codec[MasterKeys] = {
    (extendedPrivateKeyCodec withContext "bitcoinMaster") ::
      (extendedPrivateKeyCodec withContext "sideChainsMaster")
  }.as[MasterKeys]

  val walletSecretCodec: Codec[WalletSecret] = {
    (masterKeysCodec withContext "keys") ::
      (listOfN(uint8, text) withContext "mnemonic") ::
      (varsizebinarydata withContext "seed")
  }.as[WalletSecret]
}

case class WalletSecret(keys: MasterKeys, mnemonic: StringList, seed: ByteVector)
case class MasterKeys(bitcoinMaster: ExtendedPrivateKey, sideChainsMaster: ExtendedPrivateKey) {
  def sideChainMaster(chainNum: Long): ExtendedPrivateKey = derivePrivateKey(sideChainsMaster, hardened(chainNum) :: 0L :: Nil)
}