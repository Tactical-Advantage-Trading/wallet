package immortan.crypto

import immortan.crypto.Tools.Bytes
import scodec.bits.ByteVector

import javax.crypto.Cipher
import javax.crypto.spec.{IvParameterSpec, SecretKeySpec}

object AES {
  final val ivLength = 16

  def cipher(key: Bytes, initVector: Bytes, mode: Int): Cipher = {
    val aesCipher: Cipher = Cipher.getInstance("AES/CBC/PKCS5Padding")
    val ivParameterSpec: IvParameterSpec = new IvParameterSpec(initVector)
    aesCipher.init(mode, new SecretKeySpec(key, "AES"), ivParameterSpec)
    aesCipher
  }

  // Data to encode, hash256 of password, 16 byte random iv
  def decode(data: Bytes, key: Bytes, initVector: Bytes): ByteVector =
    ByteVector.view(cipher(key, initVector, Cipher.DECRYPT_MODE) doFinal data)

  def encode(data: Bytes, key: Bytes, initVector: Bytes): ByteVector =
    ByteVector.view(cipher(key, initVector, Cipher.ENCRYPT_MODE) doFinal data)
}