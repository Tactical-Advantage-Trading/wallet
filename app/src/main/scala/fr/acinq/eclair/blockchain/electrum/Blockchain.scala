package fr.acinq.eclair.blockchain.electrum

import fr.acinq.bitcoin.{BlockHeader, ByteVector32, decodeCompact}
import immortan.sqlite.SQLiteData

import java.math.BigInteger
import scala.annotation.tailrec


case class Blockchain(enforceSameBits: Boolean, checkpoints: Vector[CheckPoint], headersMap: Map[ByteVector32, Blockchain.BlockIndex],
                      bestchain: Vector[Blockchain.BlockIndex], orphans: Map[ByteVector32, BlockHeader] = Map.empty) {

  def tip = bestchain.last
  def height = if (bestchain.isEmpty) 0 else bestchain.last.height

  def getHeader(height: Int): Option[BlockHeader] = {
    val isOk = bestchain.nonEmpty && height >= bestchain.head.height && height - bestchain.head.height < bestchain.size
    if (isOk) Some(bestchain(height - bestchain.head.height).header) else None
  }
}

object Blockchain {
  val RETARGETING_PERIOD = 2016 // on bitcoin, the difficulty re-targeting period is 2016 blocks
  val MAX_REORG = 72 // we assume that there won't be a reorg of more than 72 blocks

  /**
    *
    * @param header    block header
    * @param height    block height
    * @param parent    parent block
    * @param chainwork cumulative chain work up to and including this block
    */
  case class BlockIndex(header: BlockHeader, height: Int, parent: Option[BlockIndex], chainwork: BigInt) {
    lazy val hash = header.hash

    lazy val blockId = header.blockId

    lazy val logwork = if (chainwork == 0) 0.0 else Math.log(chainwork.doubleValue) / Math.log(2.0)

    override def toString = s"BlockIndex($blockId, $height, ${parent.map(_.blockId)}, $logwork)"
  }

  def fromCheckpoints(enforceSameBits: Boolean, checkpoints: Vector[CheckPoint] = Vector.empty): Blockchain =
    Blockchain(enforceSameBits, checkpoints, Map.empty, Vector.empty)

  /**
    * Validate a chunk of 2016 headers
    *
    * Used during initial sync to batch validate
    *
    * @param height  height of the first header; must be a multiple of 2016
    * @param headers headers.
    * @throws Exception if this chunk is not valid and consistent with our checkpoints
    */
  def validateHeadersChunk(blockchain: Blockchain, height: Int, headers: Seq[BlockHeader]): Unit = {
    if (headers.isEmpty) return

    require(height % RETARGETING_PERIOD == 0, s"header chunk height $height not a multiple of 2016")
    require(BlockHeader.checkProofOfWork(headers.head))
    headers.tail.foldLeft(headers.head) {
      case (previous, current) =>
        require(BlockHeader checkProofOfWork current)
        require(current.hashPreviousBlock == previous.hash)
        // on mainnet all blocks with a re-targeting window have the same difficulty target
        // on testnet it doesn't hold, there can be a drop in difficulty if there are no blocks for 20 minutes
        if (blockchain.enforceSameBits) require(current.bits == previous.bits)
        current
    }

    val cpindex = (height / RETARGETING_PERIOD) - 1
    if (cpindex < blockchain.checkpoints.length) {
      val checkpoint = blockchain.checkpoints(cpindex)
      require(headers.head.hashPreviousBlock == checkpoint.hash)
      if (blockchain.enforceSameBits) require(headers.head.bits == checkpoint.nextBits)
    }

    if (cpindex < blockchain.checkpoints.length - 1) {
      require(headers.length == RETARGETING_PERIOD)

      val nextCheckpoint = blockchain.checkpoints(cpindex + 1)
      require(headers.last.hash == nextCheckpoint.hash)

      if (blockchain.enforceSameBits) {
        val diff = BlockHeader.calculateNextWorkRequired(headers.last, headers.head.time)
        require(diff == nextCheckpoint.nextBits)
      }
    }
  }

  def addHeadersChunk(blockchain: Blockchain, height: Int, headers: Seq[BlockHeader]): Blockchain = {
    if (headers.length > RETARGETING_PERIOD) {
      val blockchain1 = Blockchain.addHeadersChunk(blockchain, height, headers.take(RETARGETING_PERIOD))
      return Blockchain.addHeadersChunk(blockchain1, height + RETARGETING_PERIOD, headers.drop(RETARGETING_PERIOD))
    }
    if (headers.isEmpty) return blockchain
    validateHeadersChunk(blockchain, height, headers)

    height match {
      case _ if height == blockchain.checkpoints.length * RETARGETING_PERIOD =>
        // append after our last checkpoint

        // checkpoints are (block hash, * next * difficulty target), this is why:
        // - we duplicate the first checkpoints because all headers in the first chunks on mainnet had the same difficulty target
        // - we drop the last checkpoint
        val chainwork = (blockchain.checkpoints(0) +: blockchain.checkpoints.dropRight(1)).map(t => BigInt(RETARGETING_PERIOD) * Blockchain.chainWork(t.nextBits)).sum
        val blockIndex = BlockIndex(headers.head, height, None, chainwork + Blockchain.chainWork(headers.head))
        val bestchain1 = headers.tail.foldLeft(Vector(blockIndex)) {
          case (indexes, header) => indexes :+ BlockIndex(header, indexes.last.height + 1, Some(indexes.last), indexes.last.chainwork + Blockchain.chainWork(header))
        }
        val headersMap1 = blockchain.headersMap ++ bestchain1.map(bi => bi.hash -> bi)
        blockchain.copy(bestchain = bestchain1, headersMap = headersMap1)
      case _ if height < blockchain.checkpoints.length * RETARGETING_PERIOD =>
        blockchain
      case _ if height == blockchain.height + 1 =>
        // attach at our best chain
        require(headers.head.hashPreviousBlock == blockchain.bestchain.last.hash)
        val blockIndex = BlockIndex(headers.head, height, None, blockchain.bestchain.last.chainwork + Blockchain.chainWork(headers.head))
        val indexes = headers.tail.foldLeft(Vector(blockIndex)) {
          case (indexes1, header) => indexes1 :+ BlockIndex(header, indexes1.last.height + 1, Some(indexes1.last), indexes1.last.chainwork + Blockchain.chainWork(header))
        }
        val bestchain1 = blockchain.bestchain ++ indexes
        val headersMap1 = blockchain.headersMap ++ indexes.map(bi => bi.hash -> bi)
        blockchain.copy(bestchain = bestchain1, headersMap = headersMap1)
      // do nothing; headers have been validated
      case _ => throw new IllegalArgumentException(s"cannot add headers chunk to an empty blockchain: not within our checkpoint")
    }
  }

  def addHeader(blockchain: Blockchain, height: Int, header: BlockHeader): Blockchain = {
    require(BlockHeader.checkProofOfWork(header), s"invalid proof of work for $header")
    blockchain.headersMap.get(header.hashPreviousBlock) match {
      case Some(parent) if parent.height == height - 1 =>
        if (height % RETARGETING_PERIOD != 0 && blockchain.enforceSameBits) {
          // check difficulty target, which should be the same as for the parent block
          require(header.bits == parent.header.bits, s"header invalid difficulty target for $header, it should be ${parent.header.bits}")
        }

        val blockIndex = BlockIndex(header, height, Some(parent), parent.chainwork + Blockchain.chainWork(header))
        val headersMap1 = blockchain.headersMap + (blockIndex.hash -> blockIndex)
        val bestChain1 = if (parent == blockchain.bestchain.last) {
          // simplest case: we add to our current best chain
          blockchain.bestchain :+ blockIndex
        } else if (blockIndex.chainwork > blockchain.bestchain.last.chainwork) {
          // we have a new best chain
          buildChain(blockIndex)
        } else {
          blockchain.bestchain
        }
        blockchain.copy(headersMap = headersMap1, bestchain = bestChain1)
      case Some(parent) => throw new IllegalArgumentException(s"parent for $header at $height is not valid: $parent ")
      case None if height < blockchain.height - 1000 => blockchain
      case None => throw new IllegalArgumentException(s"cannot find parent for $header at $height")
    }
  }

  def addHeaders(blockchain: Blockchain, height: Int, headers: Seq[BlockHeader]): Blockchain = {
    if (headers.isEmpty) blockchain
    else if (height % RETARGETING_PERIOD == 0) addHeadersChunk(blockchain, height, headers)
    else {
      @tailrec
      def loop(bc: Blockchain, h: Int, hs: Seq[BlockHeader]): Blockchain = if (hs.isEmpty) bc else {
        loop(Blockchain.addHeader(bc, h, hs.head), h + 1, hs.tail)
      }

      loop(blockchain, height, headers)
    }
  }

  @tailrec
  def buildChain(index: BlockIndex, acc: Vector[BlockIndex] = Vector.empty[BlockIndex]): Vector[BlockIndex] =
    index.parent match { case Some(parent) => buildChain(parent, index +: acc) case None => index +: acc }

  def chainWork(target: BigInt): BigInt = BigInt(2).pow(256) / (target + BigInt(1))

  def chainWork(bits: Long): BigInt = {
    val (target, negative, overflow) = decodeCompact(bits)
    if (target == BigInteger.ZERO || negative || overflow) BigInt(0) else chainWork(target)
  }

  def chainWork(header: BlockHeader): BigInt = chainWork(header.bits)

  /**
    * Optimize blockchain
    *
    * @param acc internal accumulator
    * @return a (blockchain, indexes) tuple where headers that are old enough have been removed and new checkpoints added,
    *         and indexes is the list of header indexes that have been optimized out and must be persisted
    */
  @tailrec
  def optimize(blockchain: Blockchain, acc: Vector[BlockIndex] = Vector.empty) : (Blockchain, Vector[BlockIndex]) = {
    if (blockchain.bestchain.size >= RETARGETING_PERIOD + MAX_REORG) {
      val saveme = blockchain.bestchain.take(RETARGETING_PERIOD)
      val headersMap1 = blockchain.headersMap -- saveme.map(_.hash)
      val bestchain1 = blockchain.bestchain.drop(RETARGETING_PERIOD)
      val checkpoints1 = blockchain.checkpoints :+ CheckPoint(saveme.last.hash, bestchain1.head.header.bits)
      optimize(blockchain.copy(headersMap = headersMap1, bestchain = bestchain1, checkpoints = checkpoints1), acc ++ saveme)
    } else {
      val safeWithoutCheckpoint = blockchain.bestchain.dropRight(MAX_REORG)
      (blockchain, acc ++ safeWithoutCheckpoint)
    }
  }

  def getDifficulty(blockchain: Blockchain, height: Int, headerDb: SQLiteData): Option[Long] = {
    if (!blockchain.enforceSameBits) return None
    if (height % RETARGETING_PERIOD == 0) {
      for {
        parent <- blockchain.getHeader(height - 1) orElse headerDb.getHeader(height - 1)
        previous <- blockchain.getHeader(height - 2016) orElse headerDb.getHeader(height - 2016)
      } yield BlockHeader.calculateNextWorkRequired(parent, previous.time)
    } else {
      val hot = blockchain.getHeader(height - 1)
      val cold = headerDb.getHeader(height - 1)
      hot.orElse(cold).map(_.bits)
    }
  }
}
