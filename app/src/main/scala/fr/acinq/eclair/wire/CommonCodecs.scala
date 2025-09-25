package fr.acinq.eclair.wire

import java.net.{Inet4Address, Inet6Address, InetAddress}

import fr.acinq.bitcoin.Crypto.{PrivateKey, PublicKey}
import fr.acinq.bitcoin.DeterministicWallet.{ExtendedPrivateKey, ExtendedPublicKey, KeyPath}
import fr.acinq.bitcoin.{ByteVector32, ByteVector64, OutPoint, Transaction}
import fr.acinq.eclair.UInt64
import fr.acinq.eclair.UInt64.Conversions._
import org.apache.commons.codec.binary.Base32
import scodec.bits.{BitVector, ByteVector}
import scodec.codecs._
import scodec.{Attempt, Codec, DecodeResult, Err, SizeBound}

import scala.Ordering.Implicits._


object CommonCodecs {
  def discriminatorWithDefault[A](discriminator: Codec[A], fallback: Codec[A]): Codec[A] = new Codec[A] {
    def sizeBound: SizeBound = discriminator.sizeBound | fallback.sizeBound

    def encode(e: A): Attempt[BitVector] = discriminator.encode(e).recoverWith {
      case _ => fallback.encode(e)
    }

    def decode(b: BitVector): Attempt[DecodeResult[A]] = discriminator.decode(b).recoverWith {
      case _: KnownDiscriminatorType[_]#UnknownDiscriminator => fallback.decode(b)
    }
  }

  val uint64: Codec[UInt64] = bytes(8).xmap(UInt64.apply, _.toByteVector padLeft 8)

  val text: Codec[String] = variableSizeBytes(uint16, utf8)

  def minimalvalue[A: Ordering](codec: Codec[A], min: A): Codec[A] = codec.exmap(f = {
    case i if i < min => Attempt failure Err("value was not minimally encoded")
    case i => Attempt successful i
  }, g = Attempt.successful)

  private val m100 = UInt64(0x100000000L)
  private val k10 = UInt64(0x10000)
  private val fd = UInt64(0xfd)

  val varint: Codec[UInt64] = discriminatorWithDefault(
    discriminated[UInt64].by(uint8L)
      .\(0xff) { case i if i >= m100 => i } (minimalvalue(uint64, m100))
      .\(0xfe) { case i if i >= k10 => i } (minimalvalue(uint32.xmap(longToUint64, _.toBigInt.toLong), k10))
      .\(0xfd) { case i if i >= fd => i } (minimalvalue(uint16.xmap(intToUint64, _.toBigInt.toInt), fd)),
    uint8L.xmap(intToUint64, _.toBigInt.toInt)
  )

  val varintoverflow: Codec[Long] = varint.narrow(f = {
    case long if long <= Long.MaxValue => Attempt successful long.toBigInt.toLong
    case long => Attempt failure Err(s"overflow for value $long")
  }, longToUint64)

  val bytes32: Codec[ByteVector32] = limitedSizeBytes(codec = bytesStrict(32).xmap(ByteVector32.apply, _.bytes), limit = 32)

  val bytes64: Codec[ByteVector64] = limitedSizeBytes(codec = bytesStrict(64).xmap(ByteVector64.apply, _.bytes), limit = 64)

  val varsizebinarydata: Codec[ByteVector] = variableSizeBytes(uint16, bytes)

  val ipv4address: Codec[Inet4Address] = bytes(4).xmap(
    bv => InetAddress.getByAddress(bv.toArray).asInstanceOf[Inet4Address],
    address => ByteVector(address.getAddress)
  )

  val ipv6address: Codec[Inet6Address] = bytes(16).xmap(
    bv => Inet6Address.getByAddress(null, bv.toArray, null),
    address => ByteVector(address.getAddress)
  )

  def base32(size: Int): Codec[String] = bytes(size).xmap(
    bv => (new Base32 encodeAsString bv.toArray).toLowerCase,
    address => ByteVector(new Base32 decode address.toUpperCase)
  )

  val nodeaddress: Codec[NodeAddress] =
    discriminated[NodeAddress].by(uint8)
      .typecase(1, (ipv4address :: uint16).as[IPv4].compact)
      .typecase(2, (ipv6address :: uint16).as[IPv6].compact)
      .typecase(3, (base32(10) :: uint16).as[Tor2].compact)
      .typecase(4, (base32(35) :: uint16).as[Tor3].compact)
      .typecase(5, (zeropaddedstring(64) :: uint16).as[Domain].compact)

  type NodeAddresses = List[NodeAddress]
  val listofnodeaddresses: Codec[NodeAddresses] = variableSizeBytes(value = list(nodeaddress), size = uint16)

  val privateKey: Codec[PrivateKey] = Codec[PrivateKey](
    (privateKey: PrivateKey) => bytes(32).encode(privateKey.value),
    (wire: BitVector) => bytes(32).decode(wire).map(_ map PrivateKey.apply)
  )

  val publicKey: Codec[PublicKey] = Codec[PublicKey](
    (publicKey: PublicKey) => bytes(33).encode(publicKey.value),
    (wire: BitVector) => bytes(33).decode(wire).map(_ map PublicKey.apply)
  )

  def zeropaddedstring(size: Int): Codec[String] = fixedSizeBytes(size, utf8).xmap(_.takeWhile(_ != '\u0000'), identity)

  def lengthDelimited[T](codec: Codec[T]): Codec[T] = variableSizeBytesLong(varintoverflow, codec)

  val outPointCodec = lengthDelimited {
    bytes.xmap(d => OutPoint.read(d.toArray), OutPoint.write)
  }

  val txCodec = lengthDelimited {
    bytes.xmap(d => Transaction.read(d.toArray), Transaction.write)
  }

  val keyPathCodec: Codec[KeyPath] =
    (listOfN(uint16, uint32) withContext "path")
      .xmap[KeyPath](KeyPath.apply, _.path.toList).as[KeyPath]

  val extendedPrivateKeyCodec = {
    ("secretkeybytes" | bytes32) ::
      ("chaincode" | bytes32) ::
      ("depth" | uint16) ::
      ("path" | keyPathCodec) ::
      ("parent" | int64)
  }.as[ExtendedPrivateKey]

  val extendedPublicKeyCodec = {
    ("publickeybytes" | varsizebinarydata) ::
      ("chaincode" | bytes32) ::
      ("depth" | uint16) ::
      ("path" | keyPathCodec) ::
      ("parent" | int64)
  }.as[ExtendedPublicKey]
}
