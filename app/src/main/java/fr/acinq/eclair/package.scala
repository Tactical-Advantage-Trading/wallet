/*
 * Copyright 2019 ACINQ SAS
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package fr.acinq

import fr.acinq.bitcoin.Crypto.{PrivateKey, PublicKey}
import fr.acinq.bitcoin.DeterministicWallet.{ExtendedPrivateKey, KeyPath}
import fr.acinq.bitcoin._
import scodec.Attempt
import scodec.bits.{BitVector, ByteVector}

import java.security.SecureRandom
import scala.util.{Failure, Success, Try}


package object eclair { me =>
  val secureRandom = new SecureRandom

  def randomBytes(length: Int): ByteVector = {
    val buffer = new Array[Byte](length)
    secureRandom.nextBytes(buffer)
    ByteVector.view(buffer)
  }

  def randomBytes32: ByteVector32 = ByteVector32(me randomBytes 32)

  def randomBytes64: ByteVector64 = ByteVector64(me randomBytes 64)

  def randomKey: PrivateKey = PrivateKey(randomBytes32)

  val invalidPubKey: PublicKey = PublicKey.fromBin(ByteVector.fromValidHex("02" * 33), checkValid = false)

  val dummyExtPrivKey: ExtendedPrivateKey = ExtendedPrivateKey(randomBytes32, randomBytes32, depth = 0, KeyPath.Root, parent = 0L)

  def toLongId(fundingTxHash: ByteVector32, fundingOutputIndex: Int): ByteVector32 = {
    require(fundingOutputIndex < 65536, "fundingOutputIndex must not be greater than FFFF")
    require(fundingTxHash.size == 32, "fundingTxHash must be of length 32B")
    val channelId = ByteVector32(fundingTxHash.take(30) :+ (fundingTxHash(30) ^ (fundingOutputIndex >> 8)).toByte :+ (fundingTxHash(31) ^ fundingOutputIndex).toByte)
    channelId
  }

  def serializationResult(attempt: Attempt[BitVector]): ByteVector = attempt match {
    case Attempt.Failure(cause) => throw new RuntimeException(s"serialization error: $cause")
    case Attempt.Successful(bin) => bin.toByteVector
  }

  def proportionalFee(paymentAmount: MilliSatoshi, proportionalFee: Long): MilliSatoshi = (paymentAmount * proportionalFee) / 1000000

  def nodeFee(baseFee: MilliSatoshi, proportionalRatio: Long, paymentAmount: MilliSatoshi): MilliSatoshi = baseFee + proportionalFee(paymentAmount, proportionalRatio)

  // proportional^(exponent = 1) + ln(proportional)^(logExponent = 0) is linear
  // proportional^(exponent = 0.82) + ln(proportional)^(logExponent = 2.2) gives moderate discounts
  // proportional^(exponent = 0.79) + ln(proportional)^(logExponent = 2.1) gives substantial discounts for large amounts
  // proportional^(exponent = 0.76) + ln(proportional)^(logExponent = 2.0) gives extremely large discounts for large amounts
  // proportional^(exponent = 0) + ln(proportional)^(logExponent = 0) gives base + 2 msat, independent of payment amount
  def trampolineFee(proportional: Long, exponent: Double, logExponent: Double): MilliSatoshi = {
    val nonLinearFeeMsat = math.pow(proportional, exponent) + math.pow(math.log(proportional), logExponent)
    MilliSatoshi(nonLinearFeeMsat.ceil.toLong)
  }

  /**
   * @param address   base58 of bech32 address
   * @param chainHash hash of the chain we're on, which will be checked against the input address
   * @return the public key script that matches the input address.
   */
  def addressToPublicKeyScript(address: String, chainHash: ByteVector32): Seq[ScriptElt] = {
    Try(Base58Check.decode(address)) match {
      case Success((Base58.Prefix.PubkeyAddressTestnet, pubKeyHash)) if chainHash == Block.TestnetGenesisBlock.hash || chainHash == Block.Testnet4GenesisBlock.hash || chainHash == Block.RegtestGenesisBlock.hash => Script.pay2pkh(pubKeyHash)
      case Success((Base58.Prefix.PubkeyAddress, pubKeyHash)) if chainHash == Block.LivenetGenesisBlock.hash => Script.pay2pkh(pubKeyHash)
      case Success((Base58.Prefix.ScriptAddressTestnet, scriptHash)) if chainHash == Block.TestnetGenesisBlock.hash || chainHash == Block.Testnet4GenesisBlock.hash || chainHash == Block.RegtestGenesisBlock.hash => OP_HASH160 :: OP_PUSHDATA(scriptHash) :: OP_EQUAL :: Nil
      case Success((Base58.Prefix.ScriptAddress, scriptHash)) if chainHash == Block.LivenetGenesisBlock.hash => OP_HASH160 :: OP_PUSHDATA(scriptHash) :: OP_EQUAL :: Nil
      case Success(_) => throw new IllegalArgumentException("base58 address does not match our blockchain")
      case Failure(base58error) =>
        Try(Bech32.decodeWitnessAddress(address)) match {
          case Success((_, version, _)) if version > 1 => throw new IllegalArgumentException(s"invalid version $version in bech32 address")
          case Success((_, 0, bin)) if bin.length != 20 && bin.length != 32 => throw new IllegalArgumentException("hash length in bech32 address must be either 20 or 32 bytes")
          case Success((_, 1, bin)) if bin.length != 32 => throw new IllegalArgumentException("hash length in bech32m address must be 32 bytes")
          case Success(("bc", 1, bin)) if chainHash == Block.LivenetGenesisBlock.hash => OP_1 :: OP_PUSHDATA(bin) :: Nil
          case Success(("tb", 1, bin)) if chainHash == Block.TestnetGenesisBlock.hash || chainHash == Block.Testnet4GenesisBlock.hash => OP_1 :: OP_PUSHDATA(bin) :: Nil
          case Success(("bcrt", 1, bin)) if chainHash == Block.RegtestGenesisBlock.hash => OP_1 :: OP_PUSHDATA(bin) :: Nil
          case Success(("bc", 0, bin)) if chainHash == Block.LivenetGenesisBlock.hash => OP_0 :: OP_PUSHDATA(bin) :: Nil
          case Success(("tb", 0, bin)) if chainHash == Block.TestnetGenesisBlock.hash || chainHash == Block.Testnet4GenesisBlock.hash => OP_0 :: OP_PUSHDATA(bin) :: Nil
          case Success(("bcrt", 0, bin)) if chainHash == Block.RegtestGenesisBlock.hash => OP_0 :: OP_PUSHDATA(bin) :: Nil
          case Failure(bech32error) => throw new IllegalArgumentException(s"$address is invalid, bech32error=$bech32error/base58error=$base58error")
          case _ => throw new IllegalArgumentException(s"$address does not match our blockchain, base58error=$base58error")
        }
    }
  }

  implicit class MilliSatoshiLong(private val n: Long) extends AnyVal {
    def msat = MilliSatoshi(n)
  }

  // We implement Numeric to take advantage of operations such as sum, sort or min/max on iterables.
  implicit object NumericMilliSatoshi extends Numeric[MilliSatoshi] {
    // @formatter:off
    override def plus(x: MilliSatoshi, y: MilliSatoshi): MilliSatoshi = x + y
    override def minus(x: MilliSatoshi, y: MilliSatoshi): MilliSatoshi = x - y
    override def times(x: MilliSatoshi, y: MilliSatoshi): MilliSatoshi = MilliSatoshi(x.toLong * y.toLong)
    override def negate(x: MilliSatoshi): MilliSatoshi = -x
    override def fromInt(x: Int): MilliSatoshi = MilliSatoshi(x)
    override def toInt(x: MilliSatoshi): Int = x.toLong.toInt
    override def toLong(x: MilliSatoshi): Long = x.toLong
    override def toFloat(x: MilliSatoshi): Float = x.toLong
    override def toDouble(x: MilliSatoshi): Double = x.toLong
    override def compare(x: MilliSatoshi, y: MilliSatoshi): Int = x.compare(y)
    // @formatter:on
  }

  implicit class ToMilliSatoshiConversion(amount: Satoshi) {
    def toMilliSatoshi: MilliSatoshi = MilliSatoshi(amount.toLong * 1000L)
    def +(other: MilliSatoshi): MilliSatoshi = amount.toMilliSatoshi + other
    def -(other: MilliSatoshi): MilliSatoshi = amount.toMilliSatoshi - other
  }
}