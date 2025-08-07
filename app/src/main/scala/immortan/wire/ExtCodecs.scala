package immortan.wire

import fr.acinq.eclair.wire.CommonCodecs._
import immortan._
import scodec.Codec
import scodec.codecs._


object ExtCodecs {
  val lightningNodeKeysCodec: Codec[LightningNodeKeys] = {
    (extendedPrivateKeyCodec withContext "master") ::
    (extendedPrivateKeyCodec withContext "extendedNodeKey") ::
      (privateKey withContext "hashingKey")
  }.as[LightningNodeKeys]

  val walletSecretCodec: Codec[WalletSecret] = {
    (lightningNodeKeysCodec withContext "keys") ::
      (listOfN(uint8, text) withContext "mnemonic") ::
      (varsizebinarydata withContext "seed")
  }.as[WalletSecret]
}
