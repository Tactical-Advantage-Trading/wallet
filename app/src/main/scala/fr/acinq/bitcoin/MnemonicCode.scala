package fr.acinq.bitcoin

import org.bouncycastle.crypto.digests.SHA512Digest
import org.bouncycastle.crypto.generators.PKCS5S2ParametersGenerator
import org.bouncycastle.crypto.params.KeyParameter
import scodec.bits.ByteVector

import scala.annotation.tailrec
import scala.io.Source

/**
  * see https://github.com/bitcoin/bips/blob/master/bip-0039.mediawiki
  */
object MnemonicCode {
  lazy val englishWordlist = {
    val stream = MnemonicCode.getClass.getResourceAsStream("/bip39_english_wordlist.txt")
    Source.fromInputStream(stream, "UTF-8").getLines().toSeq
  }

  private def toBinary(x: Byte): List[Boolean] = {
    @tailrec
    def loop(x: Int, acc: List[Boolean] = List.empty[Boolean]): List[Boolean] = if (x == 0) acc else loop(x / 2, ((x % 2) != 0) :: acc)

    val digits = loop(x & 0xff)
    val zeroes = List.fill(8 - digits.length)(false)
    zeroes ++ digits
  }

  private def toBinary(x: ByteVector): List[Boolean] = x.toSeq.flatMap(toBinary).toList

  private def fromBinary(bin: Seq[Boolean]): Int = bin.foldLeft(0) { case (acc, flag) => if (flag) 2 * acc + 1 else 2 * acc }

  /**
    * BIP39 entropy encoding
    *
    * @param entropy  input entropy
    * @param wordlist word list (must be 2048 words long)
    * @return a list of mnemonic words that encodes the input entropy
    */
  def toMnemonics(entropy: ByteVector, wordlist: Seq[String] = englishWordlist): List[String] = {
    require(wordlist.length == 2048, "invalid word list (size should be 2048)")
    val digits = toBinary(entropy) ++ toBinary(Crypto.sha256(entropy)).take(entropy.length.toInt / 4)
    digits.grouped(11).map(fromBinary).map(index => wordlist(index)).toList
  }

  /**
    * validate that a mnemonic seed is valid
    *
    * @param mnemonics list of mnemomic words
    *
    */
  def validate(mnemonics: Seq[String], wordlist: Seq[String] = englishWordlist): Unit = {
    require(wordlist.length == 2048, "invalid word list (size should be 2048)")
    require(mnemonics.nonEmpty, "mnemonic code cannot be empty")
    require(mnemonics.length % 3 == 0, s"invalid mnemonic word count ${mnemonics.length}, it must be a multiple of 3")
    val wordMap = wordlist.zipWithIndex.toMap
    mnemonics.foreach(word => require(wordMap.contains(word), s"invalid mnemonic word $word"))
    val indexes = mnemonics.map(word => wordMap(word))

    @tailrec
    def toBits(index: Int, acc: Seq[Boolean] = Seq.empty[Boolean]): Seq[Boolean] = if (acc.length == 11) acc else toBits(index / 2, (index % 2 != 0) +: acc)

    val bits = indexes.flatMap(i => toBits(i))
    val bitlength = (bits.length * 32) / 33
    val (databits, checksumbits) = bits.splitAt(bitlength)
    val data = ByteVector(databits.grouped(8).map(fromBinary).map(_.toByte))
    val check = toBinary(Crypto.sha256(data)).take(data.length.toInt / 4)
    require(check == checksumbits, "invalid checksum")
  }

  def validate(mnemonics: String): Unit = validate(mnemonics.split(" ").toSeq)

  /**
    * BIP39 seed derivation
    *
    * @param mnemonics  mnemonic words
    * @param passphrase passphrase
    * @return a seed derived from the mnemonic words and passphrase
    */
  def toSeed(mnemonics: Seq[String], passphrase: String): ByteVector = {
    val gen = new PKCS5S2ParametersGenerator(new SHA512Digest())
    gen.init(mnemonics.mkString(" ").getBytes("UTF-8"), ("mnemonic" + passphrase).getBytes("UTF-8"), 2048)
    val keyParams = gen.generateDerivedParameters(512).asInstanceOf[KeyParameter]
    ByteVector.view(keyParams.getKey)
  }

  def toSeed(mnemonics: String, passphrase: String): ByteVector = toSeed(mnemonics.split(" ").toSeq, passphrase)
}
