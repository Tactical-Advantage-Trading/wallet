package immortan

import fr.acinq.bitcoin.DeterministicWallet
import fr.acinq.bitcoin.DeterministicWallet.ExtendedPrivateKey
import immortan.crypto.Tools.{Bytes, StringList}
import scodec.bits.ByteVector
import fr.acinq.eclair.wire.CommonCodecs._
import scodec.Codec
import scodec.codecs._


object MasterKeys {
  def fromSeed(seed: Bytes): MasterKeys = {
    val bitcoinMaster = DeterministicWallet.generate(ByteVector.view(seed), "Bitcoin seed")
    val tokenMaster = DeterministicWallet.generate(ByteVector.view(seed), "Token seed")
    MasterKeys(bitcoinMaster, tokenMaster)
  }

  val masterKeysCodec: Codec[MasterKeys] = {
    (extendedPrivateKeyCodec withContext "bitcoinMaster") ::
      (extendedPrivateKeyCodec withContext "tokenMaster")
  }.as[MasterKeys]

  val walletSecretCodec: Codec[WalletSecret] = {
    (masterKeysCodec withContext "keys") ::
      (listOfN(uint8, text) withContext "mnemonic") ::
      (varsizebinarydata withContext "seed")
  }.as[WalletSecret]
}

case class MasterKeys(bitcoinMaster: ExtendedPrivateKey, tokenMaster: ExtendedPrivateKey)
case class WalletSecret(keys: MasterKeys, mnemonic: StringList, seed: ByteVector)