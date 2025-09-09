package fr.acinq.bitcoin

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import java.math.BigInteger

import fr.acinq.secp256k1.{Secp256k1, Secp256k1JvmKt}
import org.bouncycastle.asn1.sec.SECNamedCurves
import org.bouncycastle.asn1.{ASN1Integer, DERSequenceGenerator}
import org.bouncycastle.crypto.Digest
import org.bouncycastle.crypto.digests.{RIPEMD160Digest, SHA1Digest, SHA256Digest, SHA512Digest}
import org.bouncycastle.crypto.macs.HMac
import org.bouncycastle.crypto.params.{ECDomainParameters, ECPrivateKeyParameters, ECPublicKeyParameters, KeyParameter}
import org.bouncycastle.crypto.signers.{ECDSASigner, HMacDSAKCalculator}
import org.bouncycastle.math.ec.ECPoint
import scodec.bits.ByteVector


object Crypto {
  val params = SECNamedCurves.getByName("secp256k1")
  val curve = new ECDomainParameters(params.getCurve, params.getG, params.getN, params.getH)
  val halfCurveOrder = params.getN().shiftRight(1)
  val zero = BigInteger.valueOf(0)
  val one = BigInteger.valueOf(1)

  private lazy val h: Secp256k1 = Secp256k1JvmKt.getSecpk256k1

  def fixSize(data: ByteVector): ByteVector32 = ByteVector32(data.padLeft(32))

  /**
   * Secp256k1 private key, which a 32 bytes value
   * We assume that private keys are compressed i.e. that the corresponding public key is compressed
   *
   * @param value value to initialize this key with
   */
  case class PrivateKey(value: ByteVector32) {
    def add(that: PrivateKey): PrivateKey = PrivateKey(ByteVector.view(h.privKeyTweakAdd(value.toArray, that.value.toArray)))

    def subtract(that: PrivateKey): PrivateKey = PrivateKey(ByteVector.view(h.privKeyTweakAdd(value.toArray, h.privKeyNegate(that.value.toArray))))

    def multiply(that: PrivateKey): PrivateKey = PrivateKey(ByteVector.view(h.privKeyTweakMul(value.toArray, that.value.toArray)))

    def +(that: PrivateKey): PrivateKey = add(that)

    def -(that: PrivateKey): PrivateKey = subtract(that)

    def *(that: PrivateKey): PrivateKey = multiply(that)

    def isZero: Boolean = bigInt == BigInteger.ZERO

    // used only if secp256k1 is not available
    lazy val bigInt = new BigInteger(1, value.toArray)

    def publicKey: PublicKey = PublicKey.fromBin(ByteVector.view(h.pubkeyCreate(value.toArray)))

    /**
     *
     * @param prefix Private key prefix
     * @return the private key in Base58 (WIF) compressed format
     */
    def toBase58(prefix: Byte) = Base58Check.encode(prefix, value.bytes :+ 1.toByte)

    def toAccount: String = s"0x${value.toHex}"
  }

  object PrivateKey {
    def apply(data: ByteVector): PrivateKey = new PrivateKey(ByteVector32(data.take(32)))

    def apply(data: BigInteger): PrivateKey = {
      new PrivateKey(fixSize(ByteVector.view(data.toByteArray.dropWhile(_ == 0.toByte))))
    }

    /**
     *
     * @param data serialized private key in bitcoin format
     * @return the de-serialized key
     */
    def fromBin(data: ByteVector): (PrivateKey, Boolean) = {
      val compressed = data.length match {
        case 32 => false
        case 33 if data.last == 1.toByte => true
      }
      (PrivateKey(data.take(32)), compressed)
    }

    def fromBase58(value: String, prefix: Byte): (PrivateKey, Boolean) = {
      require(Set(Base58.Prefix.SecretKey, Base58.Prefix.SecretKeyTestnet, Base58.Prefix.SecretKeySegnet).contains(prefix), "invalid base 58 prefix for a private key")
      val (`prefix`, data) = Base58Check.decode(value)
      fromBin(data)
    }
  }

  /**
   * Secp256k1 Public key
   * We assume that public keys are always compressed
   *
   * @param value serialized public key, in compressed format (33 bytes)
   */
  case class PublicKey(value: ByteVector) {
    require(value.length == 33)
    require(isPubKeyValidLax(value))

    def hash160: ByteVector = Crypto.hash160(value)

    def isValid: Boolean = isPubKeyValidStrict(this.value)

    def add(that: PublicKey): PublicKey = PublicKey.fromBin(ByteVector.view(h.pubKeyCombine(Array(value.toArray, that.value.toArray))))

    def add(that: PrivateKey): PublicKey = PublicKey.fromBin(ByteVector.view(h.privKeyTweakAdd(value.toArray, that.value.toArray)))

    def multiply(that: PrivateKey): PublicKey = PublicKey.fromBin(ByteVector.view(h.pubKeyTweakMul(value.toArray, that.value.toArray)))

    def *(that: PrivateKey): PublicKey = multiply(that)

    override def toString = value.toHex

    // used only if secp256k1 is not available
    lazy val ecpoint = curve.getCurve.decodePoint(value.toArray)
  }

  object PublicKey {
    def apply(data: ECPoint): PublicKey = new PublicKey(ByteVector.view(data.getEncoded(true)))

    /**
     * @param raw        serialized value of this public key (a point)
     * @param checkValid indicates whether or not we check that this is a valid public key; this should be used
     *                   carefully for optimization purposes
     * @return
     */
    def apply(raw: ByteVector, checkValid: Boolean): PublicKey = fromBin(raw, checkValid)

    def fromBin(input: ByteVector, checkValid: Boolean = true): PublicKey = {
      if (checkValid) require(isPubKeyValidStrict(input))

      input.length match {
        case 33 => PublicKey(input)
        case 65 => toCompressedUnsafe(input.toArray)
      }
    }

    /**
     * This function initializes a public key from a compressed/uncompressed representation without doing validity checks.
     *
     * This will always convert the key to its compressed representation
     *
     * Note that this mutates the input array!
     *
     * @param key 33 or 65 bytes public key (will be mutated)
     * @return an immutable compressed public key
     */
    private def toCompressedUnsafe(key: Array[Byte]): PublicKey = {
      key.length match {
        case 65 if key(0) == 4 || key(0) == 6 || key(0) == 7 =>
          key(0) = if ((key(64) & 0x01) != 0) 0x03.toByte else 0x02.toByte
          new PublicKey(ByteVector(key, 0, 33))
        case 33 if key(0) == 2 || key(0) == 3 =>
          new PublicKey(ByteVector(key, 0, 33))
        case _ =>
          throw new IllegalArgumentException(s"key must be 33 or 65 bytes")
      }
    }
  }


  /**
   * Computes ecdh using secp256k1's variant: sha256(priv * pub serialized in compressed format)
   *
   * @param priv private value
   * @param pub  public value
   * @return ecdh(priv, pub) as computed by libsecp256k1
   */
  def ecdh(priv: PrivateKey, pub: PublicKey): ByteVector32 = ByteVector32(ByteVector.view(h.ecdh(priv.value.toArray, pub.value.toArray)))

  def hmac512(key: ByteVector, data: ByteVector): ByteVector = {
    val mac = new HMac(new SHA512Digest())
    mac.init(new KeyParameter(key.toArray))
    mac.update(data.toArray, 0, data.length.toInt)
    val out = new Array[Byte](64)
    mac.doFinal(out, 0)
    ByteVector.view(out)
  }

  def hash(digest: Digest)(input: ByteVector): ByteVector = {
    digest.update(input.toArray, 0, input.length.toInt)
    val out = new Array[Byte](digest.getDigestSize)
    digest.doFinal(out, 0)
    ByteVector.view(out)
  }

  def sha1 = hash(new SHA1Digest) _

  def sha256 = (x: ByteVector) => ByteVector32(hash(new SHA256Digest)(x))

  def ripemd160 = hash(new RIPEMD160Digest) _

  /**
   * 160 bits bitcoin hash, used mostly for address encoding
   * hash160(input) = RIPEMD160(SHA256(input))
   *
   * @param input array of byte
   * @return the 160 bits BTC hash of input
   */
  def hash160(input: ByteVector): ByteVector = ripemd160(sha256(input))

  /**
   * 256 bits bitcoin hash
   * hash256(input) = SHA256(SHA256(input))
   *
   * @param input array of byte
   * @return the 256 bits BTC hash of input
   */
  def hash256(input: ByteVector): ByteVector32 = ByteVector32(sha256(sha256(input)))

  private def encodeSignatureCompact(r: BigInteger, s: BigInteger): ByteVector64 = {
    ByteVector64(ByteVector.view(r.toByteArray.dropWhile(_ == 0)).padLeft(32) ++ ByteVector.view(s.toByteArray.dropWhile(_ == 0)).padLeft(32))
  }

  def isDERSignature(sig: ByteVector): Boolean = {
    // Format: 0x30 [total-length] 0x02 [R-length] [R] 0x02 [S-length] [S] [sighash]
    // * total-length: 1-byte length descriptor of everything that follows,
    //   excluding the sighash byte.
    // * R-length: 1-byte length descriptor of the R value that follows.
    // * R: arbitrary-length big-endian encoded R value. It must use the shortest
    //   possible encoding for a positive integers (which means no null bytes at
    //   the start, except a single one when the next byte has its highest bit set).
    // * S-length: 1-byte length descriptor of the S value that follows.
    // * S: arbitrary-length big-endian encoded S value. The same rules apply.
    // * sighash: 1-byte value indicating what data is hashed (not part of the DER
    //   signature)

    // Minimum and maximum size constraints.
    if (sig.size < 9) return false
    if (sig.size > 73) return false

    // A signature is of type 0x30 (compound).
    if (sig(0) != 0x30.toByte) return false

    // Make sure the length covers the entire signature.
    if (sig(1) != sig.size - 3) return false

    // Extract the length of the R element.
    val lenR = sig(3)

    // Make sure the length of the S element is still inside the signature.
    if (5 + lenR >= sig.size) return false

    // Extract the length of the S element.
    val lenS = sig(5 + lenR)

    // Verify that the length of the signature matches the sum of the length
    // of the elements.
    if (lenR + lenS + 7 != sig.size) return false

    // Check whether the R element is an integer.
    if (sig(2) != 0x02) return false

    // Zero-length integers are not allowed for R.
    if (lenR == 0) return false

    // Negative numbers are not allowed for R.
    if ((sig(4) & 0x80.toByte) != 0) return false

    // Null bytes at the start of R are not allowed, unless R would
    // otherwise be interpreted as a negative number.
    if (lenR > 1 && (sig(4) == 0x00) && (sig(5) & 0x80) == 0) return false

    // Check whether the S element is an integer.
    if (sig(lenR + 4) != 0x02.toByte) return false

    // Zero-length integers are not allowed for S.
    if (lenS == 0) return false

    // Negative numbers are not allowed for S.
    if ((sig(lenR + 6) & 0x80) != 0) return false

    // Null bytes at the start of S are not allowed, unless S would otherwise be
    // interpreted as a negative number.
    if (lenS > 1 && (sig(lenR + 6) == 0x00) && (sig(lenR + 7) & 0x80) == 0) return false

    return true
  }

  def isLowDERSignature(sig: ByteVector): Boolean = isDERSignature(sig) && {
    val (_, s) = decodeSignatureFromDER(sig)
    s.compareTo(halfCurveOrder) <= 0
  }

  private def normalizeSignature(r: BigInteger, s: BigInteger): (BigInteger, BigInteger) = {
    val s1 = if (s.compareTo(halfCurveOrder) > 0) curve.getN().subtract(s) else s
    (r, s1)
  }

  def checkSignatureEncoding(sig: ByteVector, flags: Int): Boolean = {
    import ScriptFlags._
    // Empty signature. Not strictly DER encoded, but allowed to provide a
    // compact way to provide an invalid signature for use with CHECK(MULTI)SIG
    if (sig.isEmpty) true
    else if ((flags & (SCRIPT_VERIFY_DERSIG | SCRIPT_VERIFY_LOW_S | SCRIPT_VERIFY_STRICTENC)) != 0 && !isDERSignature(sig)) false
    else if ((flags & SCRIPT_VERIFY_LOW_S) != 0 && !isLowDERSignature(sig)) false
    else if ((flags & SCRIPT_VERIFY_STRICTENC) != 0 && !isDefinedHashtypeSignature(sig)) false
    else true
  }

  def checkPubKeyEncoding(key: ByteVector, flags: Int, sigVersion: Int): Boolean = {
    if ((flags & ScriptFlags.SCRIPT_VERIFY_STRICTENC) != 0) require(isPubKeyCompressedOrUncompressed(key), "invalid public key")
    // Only compressed keys are accepted in segwit
    if ((flags & ScriptFlags.SCRIPT_VERIFY_WITNESS_PUBKEYTYPE) != 0 && sigVersion == SigVersion.SIGVERSION_WITNESS_V0) require(isPubKeyCompressed(key), "public key must be compressed in segwit")
    true
  }

  /**
   *
   * @param key serialized public key
   * @return true if the key is valid. Please not that this performs very basic tests and does not check that the
   *         point represented by this key is actually valid.
   */
  def isPubKeyValidLax(key: ByteVector): Boolean = key.length match {
    case 65 if key(0) == 4 || key(0) == 6 || key(0) == 7 => true
    case 33 if key(0) == 2 || key(0) == 3 => true
    case _ => false
  }

  /**
   *
   * @param key serialized public key
   * @return true if the key is valid. This check is much more expensive than its lax version since here we check that
   *         the public key is a valid point on the secp256k1 curve
   */
  def isPubKeyValidStrict(key: ByteVector): Boolean = isPubKeyValidLax(key) && h.pubkeyParse(key.toArray).length == 65

  def isPubKeyCompressedOrUncompressed(key: ByteVector): Boolean = key.length match {
    case 65 if key(0) == 4 => true
    case 33 if key(0) == 2 || key(0) == 3 => true
    case _ => false
  }

  def isPubKeyCompressed(key: ByteVector): Boolean = key.length match {
    case 33 if key(0) == 2 || key(0) == 3 => true
    case _ => false
  }

  def isDefinedHashtypeSignature(sig: ByteVector): Boolean = if (sig.isEmpty) false
  else {
    val hashType = (sig.last & 0xff) & (~(SIGHASH_ANYONECANPAY))
    if (hashType < SIGHASH_ALL || hashType > SIGHASH_SINGLE) false else true
  }

  /**
   * An ECDSA signature is a (r, s) pair. Bitcoin uses DER encoded signatures
   *
   * @param blob sigbyte data
   * @return the decoded (r, s) signature
   */
  private def decodeSignatureFromDER(blob: ByteVector): (BigInteger, BigInteger) = {
    decodeSignatureFromDERLax(blob)
  }

  private def decodeSignatureFromDERLax(input: ByteArrayInputStream): (BigInteger, BigInteger) = {
    require(input.read() == 0x30)

    def readLength: Int = {
      val len = input.read()
      if ((len & 0x80) == 0) len else {
        var n = len - 0x80
        var len1 = 0
        while (n > 0) {
          len1 = (len1 << 8) + input.read()
          n = n - 1
        }
        len1
      }
    }

    readLength
    require(input.read() == 0x02)
    val lenR = readLength
    val r = new Array[Byte](lenR)
    input.read(r)
    require(input.read() == 0x02)
    val lenS = readLength
    val s = new Array[Byte](lenS)
    input.read(s)
    (new BigInteger(1, r), new BigInteger(1, s))
  }

  private def decodeSignatureFromDERLax(input: ByteVector): (BigInteger, BigInteger) = decodeSignatureFromDERLax(new ByteArrayInputStream(input.toArray))

  private def decodeSignatureCompact(signature: ByteVector64): (BigInteger, BigInteger) = {
    val r = new BigInteger(1, signature.take(32).toArray)
    val s = new BigInteger(1, signature.takeRight(32).toArray)
    (r, s)
  }

  def compact2der(signature: ByteVector64): ByteVector = {
    val r = new BigInteger(1, signature.take(32).toArray)
    val s = new BigInteger(1, signature.takeRight(32).toArray)
    val (r1, s1) = normalizeSignature(r, s)
    val bos = new ByteArrayOutputStream(73)
    val seq = new DERSequenceGenerator(bos)
    seq.addObject(new ASN1Integer(r1))
    seq.addObject(new ASN1Integer(s1))
    seq.close()
    ByteVector.view(bos.toByteArray)
  }

  def der2compact(signature: ByteVector): ByteVector64 = {
    val (r, s) = decodeSignatureFromDERLax(signature)
    val (r1, s1) = normalizeSignature(r, s)
    ByteVector64(ByteVector.view(r1.toByteArray.dropWhile(_ == 0)).padLeft(32) ++ ByteVector.view(s1.toByteArray.dropWhile(_ == 0)).padLeft(32))
  }

  /**
   * @param data      data
   * @param signature signature
   * @param publicKey public key
   * @return true is signature is valid for this data with this public key
   */
  def verifySignature(data: ByteVector, signature: ByteVector64, publicKey: PublicKey): Boolean = h.verify(signature.toArray, data.toArray, publicKey.value.toArray)

  /**
   *
   * @param privateKey private key
   * @return the corresponding public key
   */
  def publicKeyFromPrivateKey(privateKey: ByteVector) = PrivateKey(privateKey).publicKey

  /**
   * Sign data with a private key, using RCF6979 deterministic signatures
   *
   * @param data       data to sign
   * @param privateKey private key. If you are using bitcoin "compressed" private keys make sure to only use the first 32 bytes of
   *                   the key (there is an extra "1" appended to the key)
   * @return a signature in compact format (64 bytes)
   */
  def sign(data: Array[Byte], privateKey: PrivateKey): ByteVector64 = {
    val bin = h.sign(data, privateKey.value.toArray)
    ByteVector64(ByteVector.view(bin))
  }

  def sign(data: ByteVector, privateKey: PrivateKey): ByteVector64 = sign(data.toArray, privateKey)

  /**
   *
   * @param x x coordinate
   * @return a tuple (p1, p2) where p1 and p2 are points on the curve and p1.x = p2.x = x
   *         p1.y is even, p2.y is odd
   */
  private def recoverPoint(x: BigInteger): (ECPoint, ECPoint) = {
    val x1 = Crypto.curve.getCurve.fromBigInteger(x)
    val square = x1.square().add(Crypto.curve.getCurve.getA).multiply(x1).add(Crypto.curve.getCurve.getB)
    val y1 = square.sqrt()
    val y2 = y1.negate()
    val R1 = Crypto.curve.getCurve.createPoint(x1.toBigInteger, y1.toBigInteger).normalize()
    val R2 = Crypto.curve.getCurve.createPoint(x1.toBigInteger, y2.toBigInteger).normalize()
    if (y1.testBitZero()) (R2, R1) else (R1, R2)
  }

  /**
   * Recover public keys from a signature and the message that was signed. This method will return 2 public keys, and the signature
   * can be verified with both, but only one of them matches that private key that was used to generate the signature.
   *
   * @param signature signature
   * @param message   message that was signed
   * @return a recovered public key
   */
  def recoverPublicKey(signature: ByteVector64, message: ByteVector, recoveryId: Int): PublicKey = {
    val bin = h.ecdsaRecover(signature.toArray, message.toArray, recoveryId)
    PublicKey.fromBin(ByteVector.view(bin))
  }

  def recoverPublicKey(signature: ByteVector64, message: ByteVector): (PublicKey, PublicKey) = {
    val (r, s) = decodeSignatureCompact(signature)
    val m = new BigInteger(1, message.toArray)

    val (p1, p2) = recoverPoint(r)
    val Q1 = (p1.multiply(s).subtract(Crypto.curve.getG.multiply(m))).multiply(r.modInverse(Crypto.curve.getN))
    val Q2 = (p2.multiply(s).subtract(Crypto.curve.getG.multiply(m))).multiply(r.modInverse(Crypto.curve.getN))
    (PublicKey(Q1), PublicKey(Q2))
  }
}
