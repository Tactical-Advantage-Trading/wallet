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

  val varsizebinarydata: Codec[ByteVector] = variableSizeBytes(uint16, bytes)

  val privateKey: Codec[PrivateKey] = Codec[PrivateKey](
    (privateKey: PrivateKey) => bytes(32).encode(privateKey.value),
    (wire: BitVector) => bytes(32).decode(wire).map(_ map PrivateKey.apply)
  )

  val publicKey: Codec[PublicKey] = Codec[PublicKey](
    (publicKey: PublicKey) => bytes(33).encode(publicKey.value),
    (wire: BitVector) => bytes(33).decode(wire).map(_ map PublicKey.apply)
  )

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
