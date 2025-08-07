package fr.acinq.bitcoin

import java.io.{InputStream, OutputStream}

import fr.acinq.bitcoin.MerkleBlock.topHeight
import fr.acinq.bitcoin.Protocol._
import scodec.bits.ByteVector

import scala.annotation.tailrec

/**
  *
  * @param version           Block version information, based upon the software version creating this block
  * @param previousBlockHash The hash value of the previous block this particular block references
  * @param merkleRoot        The reference to a Merkle tree collection which is a hash of all transactions related to this block
  * @param timestamp         A timestamp recording when this block was created (Limited to 2106!)
  * @param bits              The calculated difficulty target being used for this block
  * @param nonce             The nonce used to generate this block… to allow variations of the header and compute different hashes
  * @param txCount           Number of transactions in the block (including unmatched ones)
  * @param hashes            hashes in depth-first order (including standard varint size prefix)
  * @param flags             flag bits, packed per 8 in a byte, least significant bit first (including standard varint size prefix)
  */
case class MerkleBlock(version: Long, previousBlockHash: ByteVector, merkleRoot: ByteVector, timestamp: Long, bits: Long, nonce: Long, txCount: Int, hashes: Seq[ByteVector], flags: ByteVector) extends BtcSerializable[MerkleBlock] {
  require(previousBlockHash.length == 32, "length of preivous block hash should be 32")
  require(merkleRoot.length == 32, "length of merkle root hash should be 32")
  hashes.foreach(h => require(h.length == 32, "length of hash should be 32"))
  require(txCount > 1, "transaction count should be greater than 1")

  override def serializer: BtcSerializer[MerkleBlock] = MerkleBlock

  def computeRoot = MerkleBlock.computeRoot(txCount, topHeight(txCount), 0, hashes, MerkleBlock.toBits(flags), Nil)
}


object MerkleBlock extends BtcSerializer[MerkleBlock] {
  override def read(input: InputStream, protocolVersion: Long): MerkleBlock = {
    val version = uint32(input)
    val previousBlockHash = hash(input)
    val merkleRoot = hash(input)
    val timestamp = uint32(input)
    val bits = uint32(input)
    val nonce = uint32(input)
    val txCount = uint32(input).toInt
    val hashCount = varint(input)
    val hashes = collection.mutable.ArrayBuffer.empty[ByteVector]
    for (i <- 0 until hashCount.toInt) hashes += hash(input)
    val flags = script(input)
    MerkleBlock(version, previousBlockHash, merkleRoot, timestamp, bits, nonce, txCount, hashes.toList, flags)
  }

  override def write(input: MerkleBlock, out: OutputStream, protocolVersion: Long) = {
    writeUInt32(input.version, out)
    writeBytes(input.previousBlockHash.toArray, out)
    writeBytes(input.merkleRoot.toArray, out)
    writeUInt32(input.timestamp, out)
    writeUInt32(input.bits, out)
    writeUInt32(input.nonce, out)
    writeUInt32(input.txCount, out)
    writeVarint(input.hashes.length, out)
    input.hashes.foreach(bin => writeBytes(bin.toArray, out))
    writeScript(input.flags.toArray, out)
  }

  def isBitSet(byte: Byte, bit: Int): Boolean = ((byte.toInt >> bit) & 0x01) != 0

  def toBits(byte: Byte): List[Boolean] = (for (i <- 0 to 7) yield isBitSet(byte, i)).toList

  def toBits(flags: ByteVector): List[Boolean] = flags.toSeq.flatMap(toBits).toList

  /**
    *
    * @param leafCount total number of leaf nodes
    * @param height    tree height (0 == bottom == leaf nodes)
    * @return the number of nodes at the given height
    */
  def calcTreeWidth(leafCount: Int, height: Int) = (leafCount + (1 << height) - 1) >> height

  /**
    *
    * @param leafCount ttoal number of leaf nodes
    * @return the height of the root node. For example if yo have 5 leaf nodes, the height of the root node is 3.
    */
  def topHeight(leafCount: Int): Int = {
    @tailrec
    def loop(height: Int): Int = if (calcTreeWidth(leafCount, height) > 1) loop(height + 1) else height

    loop(0)
  }

  /**
    * compute the root hash of a partial merkle tree:
    * Read a bit from the flag bit list:
    * If it is '0':
    * Read a hash from the hashes list, and return it as this node's hash.
    * If it is '1' and this is a leaf node:
    * Read a hash from the hashes list, store it as a matched txid, and return it as this node's hash.
    * If it is '1' and this is an internal node:
    * Descend into its left child tree, and store its computed hash as L.
    * If this node has a right child as well:
    * Descend into its right child, and store its computed hash as R.
    * If L == R, the partial merkle tree object is invalid.
    * Return Hash(L || R).
    * If this node has no right child, return Hash(L || L).
    *
    * @param count  total number of leaves
    * @param height current height (0 == bottom of the tree == leaf nodes)
    * @param pos    current position at this height, 0 = first node from the left
    * @param hashes remaining hashes to read from
    * @param bits   remaining flag bits to read from
    * @return a (hash, matched, remaining_hashes, remaining_bits) tuple where:
    *         - hash is the hash at position (height, pos)
    *         - matched is a list of matched txids and their position in the original block
    *         - remaining_hashes and remaining_bits are hashes and bits that have not been used
    */
  def computeRoot(count: Int, height: Int, pos: Int, hashes: Seq[ByteVector], bits: List[Boolean], matched: List[(ByteVector, Int)]): (ByteVector, List[(ByteVector, Int)], Seq[ByteVector], List[Boolean]) = {
    bits match {
      case false :: tail => (hashes.head, matched, hashes.tail, tail)
      case true :: tail if height == 0 => (hashes.head, (hashes.head, pos) :: matched, hashes.tail, tail)
      case true :: tail if (pos * 2 + 1) < calcTreeWidth(count, height - 1) =>
        val (left, matched1, hashes1, bits1) = computeRoot(count, height - 1, 2 * pos, hashes, tail, matched)
        val (right, matched2, hashes2, bits2) = computeRoot(count, height - 1, 2 * pos + 1, hashes1, bits1, matched1)
        require(left != right)
        (Crypto.hash256(left ++ right), matched2, hashes2, bits2)
      case true :: tail =>
        val (left, matched1, hashes1, bits1) = computeRoot(count, height - 1, 2 * pos, hashes, tail, matched)
        (Crypto.hash256(left ++ left), matched1, hashes1, bits1)
    }
  }

  def verify(merkleBlock: MerkleBlock): Unit = {
    val (root, matched, hashes, bits) = computeRoot(merkleBlock.txCount, topHeight(merkleBlock.txCount), 0, merkleBlock.hashes, toBits(merkleBlock.flags), Nil)
    require(root == merkleBlock.merkleRoot, "invalid merkle root")
    require(hashes.isEmpty)
    require(!bits.exists(b => b))
  }
}

