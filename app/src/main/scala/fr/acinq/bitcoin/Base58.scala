package fr.acinq.bitcoin

import java.nio.ByteOrder

import scodec.bits.ByteVector

/*
 * see https://en.bitcoin.it/wiki/Base58Check_encoding
 *
 * Why base-58 instead of standard base-64 encoding?
 * <ul>
 * <li>Don't want 0OIl characters that look the same in some fonts and could be used to create visually identical
 * looking account numbers.</li>
 * <li>A string with non-alphanumeric characters is not as easily accepted as an account number.</li>
 * <li>E-mail usually won't line-break if there's no punctuation to break at.</li>
 * <li>Doubleclicking selects the whole number as one word if it's all alphanumeric.</li>
 */
object Base58 {

  object Prefix {
    val PubkeyAddress = 0.toByte
    val ScriptAddress = 5.toByte
    val SecretKey = 128.toByte
    val PubkeyAddressTestnet = 111.toByte
    val ScriptAddressTestnet = 196.toByte
    val SecretKeyTestnet = 239.toByte
    val PubkeyAddressSegnet = 30.toByte
    val ScriptAddressSegnet = 50.toByte
    val SecretKeySegnet = 158.toByte
  }

}

/**
  * https://en.bitcoin.it/wiki/Base58Check_encoding
  * Base58Check is a format based on Base58 and used a lot in bitcoin, for encoding addresses and private keys for
  * example. It includes a prefix (usually a single byte) and a checksum so you know what has been encoded, and that it has
  * been transmitted correctly.
  * For example, to create an address for a public key you could write:
  * {{{
  *   val pub: BinaryData = "0202a406624211f2abbdc68da3df929f938c3399dd79fac1b51b0e4ad1d26a47aa"
  *   val address = Base58Check.encode(Base58.Prefix.PubkeyAddress, Crypto.hash160(pub))
  * }}}
  * And to decode a private key you could write:
  * {{{
  *   // check that is it a mainnet private key
  *   val (Base58.Prefix.SecretKey, priv) = Base58Check.decode("5J3mBbAH58CpQ3Y5RNJpUKPE62SQ5tfcvU2JpbnkeyhfsYB1Jcn")
  * }}}
  *
  */
object Base58Check {
  def checksum(data: ByteVector) = Crypto.hash256(data).take(4)

  /**
    * Encode data in Base58Check format.
    * For example, to create an address from a public key you could use:
    *
    * @param prefix version prefix (one byte)
    * @param data   date to be encoded
    * @return a Base58 string
    */
  def encode(prefix: Byte, data: ByteVector): String = {
    encode(ByteVector(prefix), data)
  }

  /**
    *
    * @param prefix version prefix (integer, as used with BIP32 ExtendedKeys for example)
    * @param data   data to be encoded
    * @return a Base58 String
    */
  def encode(prefix: Int, data: ByteVector): String = {
    encode(Protocol.writeUInt32(prefix, ByteOrder.BIG_ENDIAN), data)
  }

  /**
    *
    * @param prefix version prefix (several bytes, as used with BIP32 ExtendedKeys for example)
    * @param data   data to be encoded
    * @return a Base58 String
    */
  def encode(prefix: ByteVector, data: ByteVector): String = {
    val prefixAndData = prefix ++ data
    (prefixAndData ++ checksum(prefixAndData)).toBase58
  }

  /**
    * Decodes Base58 data that has been encoded with a single byte prefix
    *
    * NB: requirement check will throw an IllegalArgumentException if the checksum that is part of the encoded data cannot be verified
    *
    * @param encoded encoded data
    * @return a (prefix, data) tuple
    */
  def decode(encoded: String): (Byte, ByteVector) = {
    val (prefix, data) = decodeWithPrefixLen(encoded, 1)
    (prefix(0), data)
  }

  /**
    * Decodes Base58 data that has been encoded with an integer prefix
    *
    * NB: requirement check will throw an IllegalArgumentException if the checksum that is part of the encoded data cannot be verified
    *
    * @param encoded encoded data
    * @return a (prefix, data) tuple
    */
  def decodeWithIntPrefix(encoded: String): (Int, ByteVector) = {
    val (prefix, data) = decodeWithPrefixLen(encoded, 4)
    (Protocol.uint32(prefix.toArray, ByteOrder.BIG_ENDIAN).toInt, data)
  }

  /**
    * Decodes Base58 data that has been encoded with several bytes prefix
    *
    * NB: requirement check will throw an IllegalArgumentException if the checksum that is part of the encoded data cannot be verified
    *
    * @param encoded encoded data
    * @return a (prefix, data) tuple
    */
  def decodeWithPrefixLen(encoded: String, prefixLen: Int): (ByteVector, ByteVector) = {
    val raw = ByteVector.fromValidBase58(encoded)
    val versionAndHash = raw.dropRight(4)
    val checksum = raw.takeRight(4)
    require(checksum == Base58Check.checksum(versionAndHash), s"invalid Base58Check data $encoded")
    (versionAndHash.take(prefixLen), versionAndHash.drop(prefixLen))
  }
}