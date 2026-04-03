package fr.acinq.bitcoin

import java.io.{InputStream, OutputStream}
import java.math.BigInteger
import java.nio.ByteOrder

import fr.acinq.bitcoin.Protocol._
import scodec.bits._

object BlockHeader extends BtcSerializer[BlockHeader] {
  override def read(input: InputStream, protocolVersion: Long): BlockHeader = {
    val version = uint32(input)
    val hashPreviousBlock = hash(input)
    val hashMerkleRoot = hash(input)
    val time = uint32(input)
    val bits = uint32(input)
    val nonce = uint32(input)
    BlockHeader(version, hashPreviousBlock, hashMerkleRoot, time, bits, nonce)
  }

  override def write(input: BlockHeader, out: OutputStream, protocolVersion: Long) = {
    writeUInt32(input.version.toInt, out)
    writeBytes(input.hashPreviousBlock.toArray, out)
    writeBytes(input.hashMerkleRoot.toArray, out)
    writeUInt32(input.time.toInt, out)
    writeUInt32(input.bits.toInt, out)
    writeUInt32(input.nonce.toInt, out)
  }

  def getDifficulty(header: BlockHeader): BigInteger = {
    val nsize = header.bits >> 24
    val isneg = header.bits & 0x00800000
    val nword = header.bits & 0x007fffff
    val result = if (nsize <= 3)
      BigInteger.valueOf(nword).shiftRight(8 * (3 - nsize.toInt))
    else
      BigInteger.valueOf(nword).shiftLeft(8 * (nsize.toInt - 3))
    if (isneg != 0) result.negate() else result
  }

  /**
    *
    * @param bits difficulty target
    * @return the amount of work represented by this difficulty target, as displayed
    *         by bitcoin core
    */
  def blockProof(bits: Long): Double = {
    val (target, negative, overflow) = decodeCompact(bits)
    if (target == BigInteger.ZERO || negative || overflow) 0.0 else {
      val work = BigInteger.valueOf(2).pow(256).divide(target.add(BigInteger.ONE))
      work.doubleValue()
    }
  }

  def blockProof(header: BlockHeader): Double = blockProof(header.bits)

  /**
    * Proof of work: hash(header) <= target difficulty
    *
    * @param header block header
    * @return true if the input block header validates its expected proof of work
    */
  def checkProofOfWork(header: BlockHeader): Boolean = {
    val (target, _, _) = decodeCompact(header.bits)
    val hash = new BigInteger(1, header.blockId.toArray)
    hash.compareTo(target) <= 0
  }

  def calculateNextWorkRequired(lastHeader: BlockHeader, lastRetargetTime: Long): Long = {
    var actualTimespan = lastHeader.time - lastRetargetTime
    val targetTimespan = 14 * 24 * 60 * 60 // two weeks
    if (actualTimespan < targetTimespan / 4) actualTimespan = targetTimespan / 4
    if (actualTimespan > targetTimespan * 4) actualTimespan = targetTimespan * 4

    var (target, false, false) = decodeCompact(lastHeader.bits)
    target = target.multiply(BigInteger.valueOf(actualTimespan))
    target = target.divide(BigInteger.valueOf(targetTimespan))

    val powLimit = new BigInteger(1, hex"00000000ffffffffffffffffffffffffffffffffffffffffffffffffffffffff".toArray)
    target = target.min(powLimit)
    encodeCompact(target)
  }
}

/**
  *
  * @param version           Block version information, based upon the software version creating this block
  * @param hashPreviousBlock The hash value of the previous block this particular block references. Please not that
  *                          this hash is not reversed (as opposed to Block.hash)
  * @param hashMerkleRoot    The reference to a Merkle tree collection which is a hash of all transactions related to this block
  * @param time              A timestamp recording when this block was created (Will overflow in 2106[2])
  * @param bits              The calculated difficulty target being used for this block
  * @param nonce             The nonce used to generate this block… to allow variations of the header and compute different hashes
  */
case class BlockHeader(version: Long, hashPreviousBlock: ByteVector32, hashMerkleRoot: ByteVector32, time: Long, bits: Long, nonce: Long) extends BtcSerializable[BlockHeader] {
  lazy val hash: ByteVector32 = Crypto.hash256(BlockHeader.write(this))

  lazy val blockId = hash.reverse

  override def serializer: BtcSerializer[BlockHeader] = BlockHeader
}

object Block extends BtcSerializer[Block] {
  override def read(input: InputStream, protocolVersion: Long): Block = {
    val raw = bytes(input, 80)
    val header = BlockHeader.read(raw.toArray)
    Block(header, readCollection[Transaction](input, protocolVersion))
  }

  override def write(input: Block, out: OutputStream, protocolVersion: Long) = {
    BlockHeader.write(input.header, out)
    writeCollection(input.tx, out, protocolVersion)
  }

  override def validate(input: Block): Unit = {
    BlockHeader.validate(input.header)
    require(input.header.hashMerkleRoot === MerkleTree.computeRoot(input.tx.map(_.hash)), "invalid block:  merkle root mismatch")
    require(input.tx.map(_.txid).toSet.size == input.tx.size, "invalid block: duplicate transactions")
    input.tx.foreach(Transaction.validate)
  }

  def blockProof(block: Block): Double = BlockHeader.blockProof(block.header)

  // genesis blocks
  val LivenetGenesisBlock = {
    val script = OP_PUSHDATA(writeUInt32(486604799L)) :: OP_PUSHDATA(hex"04") :: OP_PUSHDATA(ByteVector("The Times 03/Jan/2009 Chancellor on brink of second bailout for banks".getBytes("UTF-8"))) :: Nil
    val scriptPubKey = OP_PUSHDATA(hex"04678afdb0fe5548271967f1a67130b7105cd6a828e03909a67962e0ea1f61deb649f6bc3f4cef38c4f35504e51ec112de5c384df7ba0b8d578a4c702b6bf11d5f") :: OP_CHECKSIG :: Nil
    Block(
      BlockHeader(version = 1, hashPreviousBlock = ByteVector32.Zeroes, hashMerkleRoot = ByteVector32(hex"3ba3edfd7a7b12b27ac72c3e67768f617fc81bc3888a51323a9fb8aa4b1e5e4a"), time = 1231006505, bits = 0x1d00ffff, nonce = 2083236893),
      List(
        Transaction(version = 1,
          txIn = List(TxIn.coinbase(script)),
          txOut = List(TxOut(amount = 50.btc, publicKeyScript = scriptPubKey)),
          lockTime = 0))
    )
  }

  val TestnetGenesisBlock = LivenetGenesisBlock.copy(header = LivenetGenesisBlock.header.copy(time = 1296688602, nonce = 414098458))

  val RegtestGenesisBlock = LivenetGenesisBlock.copy(header = LivenetGenesisBlock.header.copy(bits = 0x207fffffL, nonce = 2, time = 1296688602))

  def checkProofOfWork(block: Block): Boolean = BlockHeader.checkProofOfWork(block.header)
}

case class Block(header: BlockHeader, tx: Seq[Transaction] = Nil) extends BtcSerializable[Block] {
  override def serializer: BtcSerializer[Block] = Block
  lazy val hash = header.hash
}

