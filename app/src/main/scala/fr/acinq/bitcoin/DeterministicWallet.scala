package fr.acinq.bitcoin

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, OutputStream}
import java.math.BigInteger
import java.nio.ByteOrder

import fr.acinq.bitcoin.Crypto.{PrivateKey, PublicKey}
import fr.acinq.bitcoin.Protocol._
import scodec.bits.ByteVector

/**
 * see https://github.com/bitcoin/bips/blob/master/bip-0032.mediawiki
 */
object DeterministicWallet {

  case class KeyPath(path: Seq[Long]) {
    def lastChildNumber: Long = if (path.isEmpty) 0L else path.last

    def derive(number: Long) = KeyPath(path :+ number)

    override def toString = path.map(KeyPath.childNumberToString).foldLeft("m")(_ + "/" + _)
  }

  object KeyPath {
    val Root = KeyPath(Nil)

    /**
     *
     * @param path key path. A list of integers separated by a `/`. May start with "/" or "m/". A single quote appended
     *             at the end means use the hardened version of the ley index (example: m/44'/0'/0'/0)
     * @return a KeyPath instance
     */
    def apply(path: String): KeyPath = {
      def toNumber(value: String): Long = if (value.last == '\'') hardened(value.dropRight(1).toLong) else value.toLong

      val path1 = path.stripPrefix("m").stripPrefix("/")
      if (path1.isEmpty) KeyPath.Root else new KeyPath(path1.split('/').map(toNumber).toSeq)
    }

    def childNumberToString(childNumber: Long) = if (isHardened(childNumber)) (childNumber - hardenedKeyIndex).toString + "'" else childNumber.toString
  }

  implicit def keypath2longseq(input: KeyPath): Seq[Long] = input.path

  implicit def longseq2keypath(input: Seq[Long]): KeyPath = KeyPath(input)

  val hardenedKeyIndex = 0x80000000L

  def hardened(index: Long): Long = hardenedKeyIndex + index

  def isHardened(index: Long): Boolean = index >= hardenedKeyIndex

  case class ExtendedPrivateKey(secretkeybytes: ByteVector32, chaincode: ByteVector32, depth: Int, path: KeyPath, parent: Long) {

    def privateKey: PrivateKey = PrivateKey(secretkeybytes)

    def publicKey: PublicKey = privateKey.publicKey
  }

  object ExtendedPrivateKey {
    def decode(input: String, parentPath: KeyPath = KeyPath.Root): (Int, ExtendedPrivateKey) = {
      val (prefix, bin) = Base58Check.decodeWithIntPrefix(input)
      val bis = new ByteArrayInputStream(bin.toArray)
      val depth = Protocol.uint8(bis)
      val parent = Protocol.uint32(bis, ByteOrder.BIG_ENDIAN)
      val childNumber = Protocol.uint32(bis, ByteOrder.BIG_ENDIAN)
      val chaincode = ByteVector32(Protocol.bytes(bis, 32))
      require(bis.read() == 0)
      val secretkeybytes = ByteVector32(Protocol.bytes(bis, 32))
      (prefix, ExtendedPrivateKey(secretkeybytes, chaincode, depth, parentPath.derive(childNumber), parent))
    }
  }

  def encode(input: ExtendedPrivateKey, prefix: Int): String = {
    val out = new ByteArrayOutputStream()
    writeUInt8(input.depth, out)
    writeUInt32(input.parent.toInt, out, ByteOrder.BIG_ENDIAN)
    writeUInt32(input.path.lastChildNumber.toInt, out, ByteOrder.BIG_ENDIAN)
    out.write(input.chaincode.toArray)
    out.write(0)
    out.write(input.secretkeybytes.toArray)
    val buffer = ByteVector.view(out.toByteArray)
    Base58Check.encode(prefix, buffer)
  }

  case class ExtendedPublicKey(publickeybytes: ByteVector, chaincode: ByteVector32, depth: Int, path: KeyPath, parent: Long) {
    lazy val publicKey: PublicKey = PublicKey(publickeybytes)
    require(publickeybytes.length == 33)
    require(chaincode.length == 32)
  }

  object ExtendedPublicKey {
    def decode(input: String, parentPath: KeyPath = KeyPath.Root): (Int, ExtendedPublicKey) = {
      val (prefix, bin) = Base58Check.decodeWithIntPrefix(input)
      val bis = new ByteArrayInputStream(bin.toArray)
      val depth = Protocol.uint8(bis)
      val parent = Protocol.uint32(bis, ByteOrder.BIG_ENDIAN)
      val childNumber = Protocol.uint32(bis, ByteOrder.BIG_ENDIAN)
      val chaincode = ByteVector32(Protocol.bytes(bis, 32))
      val publickeybytes = Protocol.bytes(bis, 33)
      (prefix.toInt, ExtendedPublicKey(publickeybytes, chaincode, depth, parentPath.derive(childNumber), parent))
    }
  }

  def encode(input: ExtendedPublicKey, prefix: Int): String = {
    val out = new ByteArrayOutputStream()
    write(input, out)
    val buffer = ByteVector.view(out.toByteArray)
    Base58Check.encode(prefix, buffer)
  }

  def write(input: ExtendedPublicKey, output: OutputStream): Unit = {
    writeUInt8(input.depth, output)
    writeUInt32(input.parent.toInt, output, ByteOrder.BIG_ENDIAN)
    writeUInt32(input.path.lastChildNumber.toInt, output, ByteOrder.BIG_ENDIAN)
    writeBytes(input.chaincode.toArray, output)
    writeBytes(input.publickeybytes.toArray, output)
  }

  /**
   *
   * @param seed random seed
   * @return a "master" private key
   */
  def generate(seed: ByteVector): ExtendedPrivateKey = {
    val I = Crypto.hmac512(ByteVector.view("Bitcoin seed".getBytes("UTF-8")), seed)
    val IL = ByteVector32(I.take(32))
    val IR = ByteVector32(I.takeRight(32))
    ExtendedPrivateKey(IL, IR, depth = 0, path = List.empty[Long], parent = 0L)
  }

  /**
   *
   * @param input extended private key
   * @return the public key for this private key
   */
  def publicKey(input: ExtendedPrivateKey): ExtendedPublicKey = {
    ExtendedPublicKey(input.publicKey.value, input.chaincode, depth = input.depth, path = input.path, parent = input.parent)
  }

  /**
   *
   * @param input extended public key
   * @return the fingerprint for this public key
   */
  def fingerprint(input: ExtendedPublicKey): Long = uint32(new ByteArrayInputStream(Crypto.hash160(input.publickeybytes).take(4).reverse.toArray))

  /**
   *
   * @param input extended private key
   * @return the fingerprint for this private key (which is based on the corresponding public key)
   */
  def fingerprint(input: ExtendedPrivateKey): Long = fingerprint(publicKey(input))

  /**
   *
   * @param parent extended private key
   * @param index  index of the child key
   * @return the derived private key at the specified index
   */
  def derivePrivateKey(parent: ExtendedPrivateKey, index: Long): ExtendedPrivateKey = {
    val I = if (isHardened(index)) {
      val buffer = 0.toByte +: parent.secretkeybytes
      Crypto.hmac512(parent.chaincode, buffer ++ writeUInt32(index.toInt, ByteOrder.BIG_ENDIAN))
    } else {
      val pub = publicKey(parent).publickeybytes
      Crypto.hmac512(parent.chaincode, pub ++ writeUInt32(index.toInt, ByteOrder.BIG_ENDIAN))
    }
    val IL = ByteVector32(I.take(32))
    val IR = ByteVector32(I.takeRight(32))
    val p = new BigInteger(1, IL.toArray)
    if (p.compareTo(Crypto.curve.getN) >= 0) {
      throw new RuntimeException("cannot generated child private key")
    }

    val key = PrivateKey(IL).add(parent.privateKey)
    if (key.isZero) {
      throw new RuntimeException("cannot generated child private key")
    }
    val buffer = key.value
    ExtendedPrivateKey(buffer, chaincode = IR, depth = parent.depth + 1, path = parent.path.derive(index), parent = fingerprint(parent))
  }

  /**
   *
   * @param parent extended public key
   * @param index  index of the child key
   * @return the derived public key at the specified index
   */
  def derivePublicKey(parent: ExtendedPublicKey, index: Long): ExtendedPublicKey = {
    require(!isHardened(index), "Cannot derive public keys from public hardened keys")

    val I = Crypto.hmac512(parent.chaincode, parent.publickeybytes ++ writeUInt32(index.toInt, ByteOrder.BIG_ENDIAN))
    val IL = ByteVector32(I.take(32))
    val IR = ByteVector32(I.takeRight(32))
    val p = new BigInteger(1, IL.toArray)
    if (p.compareTo(Crypto.curve.getN) >= 0) {
      throw new RuntimeException("cannot generated child public key")
    }
    val Ki = PrivateKey(p).publicKey.add(parent.publicKey)
    if (Ki.ecpoint.isInfinity) {
      throw new RuntimeException("cannot generated child public key")
    }
    val buffer = Ki.value
    ExtendedPublicKey(buffer, chaincode = IR, depth = parent.depth + 1, path = parent.path.derive(index), parent = fingerprint(parent))
  }

  def derivePrivateKey(parent: ExtendedPrivateKey, chain: Seq[Long]): ExtendedPrivateKey = chain.foldLeft(parent)(derivePrivateKey)

  def derivePrivateKey(parent: ExtendedPrivateKey, keyPath: KeyPath): ExtendedPrivateKey = derivePrivateKey(parent, keyPath.path)

  def derivePublicKey(parent: ExtendedPublicKey, chain: Seq[Long]): ExtendedPublicKey = chain.foldLeft(parent)(derivePublicKey)

  def derivePublicKey(parent: ExtendedPublicKey, keyPath: KeyPath): ExtendedPublicKey = derivePublicKey(parent, keyPath.path)

  // p2pkh mainnet
  val xprv = 0x0488ade4
  val xpub = 0x0488b21e

  // p2sh-of-p2wpkh mainnet
  val yprv = 0x049d7878
  val ypub = 0x049d7cb2

  // p2wpkh mainnet
  val zprv = 0x04b2430c
  val zpub = 0x04b24746

  // p2pkh testnet
  val tprv = 0x04358394
  val tpub = 0x043587cf

  // p2sh-of-p2wpkh testnet
  val uprv = 0x044a4e28
  val upub = 0x044a5262

  // p2wpkh testnet
  val vprv = 0x045f18bc
  val vpub = 0x045f1cf6
}

