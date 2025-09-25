package fr.acinq.bitcoin

import scodec.bits.ByteVector

/**
  * Bech32 and Bech32m address formats.
  * See https://github.com/bitcoin/bips/blob/master/bip-0173.mediawiki and https://github.com/bitcoin/bips/blob/master/bip-0350.mediawiki.
  */
object Bech32 {
  val alphabet = "qpzry9x8gf2tvdw0s3jn54khce6mua7l"

  // @formatter:off
  sealed trait Encoding
  case object Bech32Encoding extends Encoding
  case object Bech32mEncoding extends Encoding
  // @formatter:on

  // 5 bits integer
  // Bech32 works with 5 bits values, we use this type to make it explicit: whenever you see Int5 it means 5 bits values,
  // and whenever you see Byte it means 8 bits values
  type Int5 = Byte

  // char -> 5 bits value
  private val InvalidChar = 255.toByte
  val map = {
    val result = new Array[Int5](255)
    for (i <- result.indices) result(i) = InvalidChar
    alphabet.zipWithIndex.foreach { case (c, i) => result(c) = i.toByte }
    result
  }

  private def expand(hrp: String): Array[Int5] = {
    val result = new Array[Int5](2 * hrp.length + 1)
    var i = 0
    while (i < hrp.length) {
      result(i) = (hrp(i).toInt >>> 5).toByte
      result(hrp.length() + 1 + i) = (hrp(i).toInt & 31).toByte
      i = i + 1
    }
    result(hrp.length()) = 0.toByte
    result
  }

  private def polymod(values: Array[Int5], values1: Array[Int5]): Int = {
    val GEN = Array(0x3b6a57b2, 0x26508e6d, 0x1ea119fa, 0x3d4233dd, 0x2a1462b3)
    var chk = 1
    values.foreach(v => {
      val b = chk >>> 25
      chk = ((chk & 0x1ffffff) << 5) ^ v
      for (i <- 0 until 5) {
        if (((b >>> i) & 1) != 0) chk = chk ^ GEN(i)
      }
    })
    values1.foreach(v => {
      val b = chk >>> 25
      chk = ((chk & 0x1ffffff) << 5) ^ v
      for (i <- 0 until 5) {
        if (((b >>> i) & 1) != 0) chk = chk ^ GEN(i)
      }
    })
    chk
  }

  /**
    * @param hrp   human readable prefix
    * @param int5s 5-bit data
    * @return hrp + data encoded as a Bech32 string
    */
  def encode(hrp: String, int5s: Array[Int5], encoding: Encoding): String = {
    require(hrp.toLowerCase == hrp || hrp.toUpperCase == hrp, "mixed case strings are not valid bech32 prefixes")
    val checksum = Bech32.checksum(hrp, int5s, encoding)
    hrp + "1" + new String((int5s ++ checksum).map(i => alphabet(i)))
  }

  /**
    * decodes a bech32 or bech32m string
    *
    * @param bech32 bech32 or bech32m string
    * @return a (encoding, hrp, data) tuple
    */
  def decode(bech32: String): (String, Array[Int5], Encoding) = {
    require(bech32.toLowerCase == bech32 || bech32.toUpperCase == bech32, "mixed case strings are not valid bech32")
    bech32.foreach(c => require(c >= 33 && c <= 126, "invalid character"))
    val input = bech32.toLowerCase()
    val pos = input.lastIndexOf('1')
    val hrp = input.take(pos)
    require(hrp.nonEmpty && hrp.length <= 83, "hrp must contain 1 to 83 characters")
    val data = new Array[Int5](input.length - pos - 1)
    for (i <- data.indices) {
      val elt = map(input(pos + 1 + i))
      require(elt != InvalidChar, s"invalid bech32 character ${input(pos + 1 + i)}")
      data(i) = elt
    }
    val checksum = polymod(expand(hrp), data)
    val encoding = checksum match {
      case 1 => Bech32Encoding
      case 0x2bc830a3 => Bech32mEncoding
      case _ => throw new IllegalArgumentException(s"invalid checksum for $bech32")
    }
    (hrp, data.dropRight(6), encoding)
  }

  /**
    * @param hrp      Human Readable Part
    * @param data     data (a sequence of 5 bits integers)
    * @param encoding encoding to use (bech32 or bech32m)
    * @return a checksum computed over hrp and data
    */
  private def checksum(hrp: String, data: Array[Int5], encoding: Encoding): Array[Int5] = {
    val constant = encoding match {
      case Bech32Encoding => 1
      case Bech32mEncoding => 0x2bc830a3
    }
    val values = expand(hrp) ++ data
    val poly = polymod(values, Array(0.toByte, 0.toByte, 0.toByte, 0.toByte, 0.toByte, 0.toByte)) ^ constant
    val result = new Array[Int5](6)
    for (i <- 0 to 5) result(i) = ((poly >>> 5 * (5 - i)) & 31).toByte
    result
  }

  /**
    * @param input a sequence of 8 bits integers
    * @return a sequence of 5 bits integers
    */
  def eight2five(input: Array[Byte]): Array[Int5] = {
    var buffer = 0L
    val output = collection.mutable.ArrayBuffer.empty[Byte]
    var count = 0
    input.foreach(b => {
      buffer = (buffer << 8) | (b & 0xff)
      count = count + 8
      while (count >= 5) {
        output.append(((buffer >> (count - 5)) & 31).toByte)
        count = count - 5
      }
    })
    if (count > 0) output.append(((buffer << (5 - count)) & 31).toByte)
    output.toArray
  }

  /**
    * @param input a sequence of 5 bits integers
    * @return a sequence of 8 bits integers
    */
  def five2eight(input: Array[Int5]): Array[Byte] = {
    var buffer = 0L
    val output = collection.mutable.ArrayBuffer.empty[Byte]
    var count = 0
    input.foreach(b => {
      buffer = (buffer << 5) | (b & 31)
      count = count + 5
      while (count >= 8) {
        output.append(((buffer >> (count - 8)) & 0xff).toByte)
        count = count - 8
      }
    })
    require(count <= 4, "Zero-padding of more than 4 bits")
    require((buffer & ((1 << count) - 1)) == 0, "Non-zero padding in 8-to-5 conversion")
    output.toArray
  }

  /**
    * encode a bitcoin witness address
    *
    * @param hrp            should be "bc" or "tb"
    * @param witnessVersion witness version (0 to 16)
    * @param data           witness program: if version is 0, either 20 bytes (P2WPKH) or 32 bytes (P2WSH)
    * @return a bech32 encoded witness address
    */
  def encodeWitnessAddress(hrp: String, witnessVersion: Byte, data: ByteVector): String = {
    require(0 <= witnessVersion && witnessVersion <= 16, "invalid segwit version")
    val encoding = witnessVersion match {
      case 0 => Bech32Encoding
      case _ => Bech32mEncoding
    }
    val data1 = witnessVersion +: Bech32.eight2five(data.toArray)
    val checksum = Bech32.checksum(hrp, data1, encoding)
    hrp + "1" + new String((data1 ++ checksum).map(i => alphabet(i)))
  }

  /**
    * decode a bitcoin witness address
    *
    * @param address witness address
    * @return a (prefix, version, program) tuple where prefix is the human-readable prefix, version
    *         is the witness version and program the decoded witness program.
    *         If version is 0, it will be either 20 bytes (P2WPKH) or 32 bytes (P2WSH).
    */
  def decodeWitnessAddress(address: String): (String, Byte, ByteVector) = {
    if (address.indexWhere(_.isLower) != -1 && address.indexWhere(_.isUpper) != -1) throw new IllegalArgumentException("input mixes lowercase and uppercase characters")
    val (hrp, data, encoding) = decode(address)
    require(hrp == "bc" || hrp == "tb" || hrp == "bcrt", s"invalid HRP $hrp")
    val version = data(0)
    require(version >= 0 && version <= 16, "invalid segwit version")
    val bin = five2eight(data.drop(1))
    require(bin.length >= 2 && bin.length <= 40, s"invalid witness program length ${bin.length}")
    if (version == 0) require(encoding == Bech32Encoding, "version 0 must be encoded with Bech32")
    if (version == 0) require(bin.length == 20 || bin.length == 32, s"invalid witness program length ${bin.length}")
    if (version != 0) require(encoding == Bech32mEncoding, "version 1 to 16 must be encoded with Bech32m")
    (hrp, version, ByteVector.view(bin))
  }

}
