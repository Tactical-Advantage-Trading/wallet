package fr.acinq.eclair.blockchain.electrum

import fr.acinq.bitcoin.{BlockHeader, ByteVector32, decodeCompact}
import trading.tacticaladvantage.sqlite.SQLiteData

import java.math.BigInteger
import scala.annotation.tailrec


case class Blockchain(enforceSameBits: Boolean,
                      checkpoints: Vector[CheckPoint],
                      headersMap: Map[ByteVector32, Blockchain.BlockIndex],
                      bestchain: Vector[Blockchain.BlockIndex] = Vector.empty) {

  lazy val tip: Blockchain.BlockIndex = bestchain.last
  val height: Int = bestchain.lastOption.fold(0)(_.height)

  def getHeader(height: Int): Option[BlockHeader] = {
    val isOk = bestchain.nonEmpty && height >= bestchain.head.height && height - bestchain.head.height < bestchain.size
    if (isOk) Some(bestchain(height - bestchain.head.height).header) else None
  }

  def isReorg(height1: Int, header: BlockHeader, headerDb: SQLiteData): Boolean = {
    val replacesKnownHistoricalHeader = getHeader(height1).exists(_.hash != header.hash)
    val doesNotExtendTip = bestchain.nonEmpty && height1 == tip.height + 1 && header.hashPreviousBlock != tip.hash
    val difficultyChecksOut = Blockchain.getDifficulty(blockchain = this, height1, headerDb).forall(header.bits.==)
    (replacesKnownHistoricalHeader || doesNotExtendTip) && BlockHeader.checkProofOfWork(header) && difficultyChecksOut
  }
}

object Blockchain {
  type BlockIdxVec = Vector[BlockIndex]
  val RETARGETING_PERIOD = 2016
  val MAX_REORG = 72

  case class BlockIndex(header: BlockHeader, height: Int, parent: Option[BlockIndex] = None, chainwork: BigInt = 0) {
    lazy val hash = header.hash
  }

  @tailrec
  private def ancestorAt(index: BlockIndex, height: Int): Option[BlockIndex] =
    if (index.height == height) Some(index)
    else if (index.height < height) None
    else index.parent match {
      case Some(parent) =>
        ancestorAt(parent, height)
      case None => None
    }

  private def checkpointNextBits(blockchain: Blockchain, height: Int): Option[Long] = {
    val cpindex = (height / RETARGETING_PERIOD) - 1
    if (cpindex >= 0 && cpindex < blockchain.checkpoints.length)
      Some(blockchain.checkpoints(cpindex).nextBits) else None
  }

  private def expectedBits(blockchain: Blockchain, height: Int, parent: BlockIndex): Option[Long] =
    if (height % RETARGETING_PERIOD != 0) Some(parent.header.bits)
    else checkpointNextBits(blockchain, height).orElse {
      ancestorAt(parent, height - RETARGETING_PERIOD).map { previous =>
        BlockHeader.calculateNextWorkRequired(parent.header, previous.header.time)
      }
    }

  def validateHeadersChunk(blockchain: Blockchain, height: Int, headers: Seq[BlockHeader] = Nil): Unit = {
    if (headers.isEmpty) return

    require(height % RETARGETING_PERIOD == 0, "e1")
    val cpindex = (height / RETARGETING_PERIOD) - 1
    require(BlockHeader checkProofOfWork headers.head, "e2")

    headers.tail.foldLeft(headers.head) {
      case (previous, current) =>
        require(BlockHeader checkProofOfWork current, "e3")
        require(current.hashPreviousBlock == previous.hash, "e4")
        // on mainnet all blocks with a re-targeting window have the same difficulty target
        // on testnet it doesn't hold, there can be a drop in difficulty if there are no blocks for 20 minutes
        if (blockchain.enforceSameBits) require(current.bits == previous.bits, "e5")
        current
    }

    if (cpindex < blockchain.checkpoints.length) {
      val checkpoint = blockchain.checkpoints(cpindex)
      require(headers.head.hashPreviousBlock == checkpoint.hash, "e6")
      if (blockchain.enforceSameBits) require(headers.head.bits == checkpoint.nextBits, "e7")
    } else if (blockchain.enforceSameBits) {
      val parent = blockchain.headersMap.getOrElse(headers.head.hashPreviousBlock, throw new IllegalArgumentException)
      val expected = expectedBits(blockchain, height, parent).getOrElse(throw new IllegalArgumentException)
      require(headers.head.bits == expected, "e8")
      require(parent.height == height - 1, "e9")
    }

    if (cpindex < blockchain.checkpoints.length - 1) {
      val nextCheckpoint = blockchain.checkpoints(cpindex + 1)
      require(headers.last.hash == nextCheckpoint.hash, "e10")
      require(headers.length == RETARGETING_PERIOD, "e11")

      if (blockchain.enforceSameBits) {
        val diff = BlockHeader.calculateNextWorkRequired(headers.last, headers.head.time)
        require(diff == nextCheckpoint.nextBits, "e12")
      }
    }
  }

  def doAddHeaders(start: BlockIdxVec, headers: Seq[BlockHeader] = Nil): BlockIdxVec = headers.tail.foldLeft(start) {
    case (indexes, header) => indexes :+ BlockIndex(header, indexes.last.height + 1, Some(indexes.last), chainWork(header.bits) + indexes.last.chainwork)
  }

  def addHeadersChunk(blockchain: Blockchain, height: Int, headers: Seq[BlockHeader] = Nil): Blockchain = {
    if (headers.length > RETARGETING_PERIOD) {
      val blockchain1 = addHeadersChunk(blockchain, height, headers take RETARGETING_PERIOD)
      return addHeadersChunk(blockchain1, height + RETARGETING_PERIOD, headers drop RETARGETING_PERIOD)
    }

    if (headers.isEmpty) return blockchain
    validateHeadersChunk(blockchain, height, headers)

    height match {
      case _ if height == blockchain.checkpoints.length * RETARGETING_PERIOD =>
        val workSpan = blockchain.checkpoints(0) +: blockchain.checkpoints.dropRight(1)
        val chainwork = workSpan.map(_.nextBits).map(chainWork).map(bits => BigInt(RETARGETING_PERIOD) * bits)
        val blockIndex = BlockIndex(headers.head, height, None, chainWork(headers.head.bits) + chainwork.sum)

        val bestchain1 = doAddHeaders(Vector(blockIndex), headers)
        val headersMap1 = blockchain.headersMap ++ bestchain1.map(bi => bi.hash -> bi)
        blockchain.copy(bestchain = bestchain1, headersMap = headersMap1)

      case _ if height < blockchain.checkpoints.length * RETARGETING_PERIOD =>
        blockchain

      case _ if height == blockchain.height + 1 =>
        require(headers.head.hashPreviousBlock == blockchain.bestchain.last.hash, "e13")
        val cumulative = chainWork(headers.head.bits) + blockchain.bestchain.last.chainwork
        val blockIndex = BlockIndex(headers.head, height, None, cumulative)

        val indexes = doAddHeaders(Vector(blockIndex), headers)
        val headersMap1 = blockchain.headersMap ++ indexes.map(bi => bi.hash -> bi)
        blockchain.copy(bestchain = blockchain.bestchain ++ indexes, headersMap = headersMap1)

      case _ =>
        throw new IllegalArgumentException
    }
  }

  def addHeader(blockchain: Blockchain, height: Int, header: BlockHeader): Blockchain = {
    require(blockchain.bestchain.nonEmpty && header.hashPreviousBlock == blockchain.tip.hash, "e14")
    require(BlockHeader checkProofOfWork header, "e15")
    require(height == blockchain.height + 1, "e16")

    if (blockchain.enforceSameBits) {
      val expected = expectedBits(blockchain, height, blockchain.tip)
      require(expected.getOrElse(throw new IllegalArgumentException) == header.bits, "e17")
    }

    val blockIndex = BlockIndex(header = header, height = height, parent = Some(blockchain.tip), chainwork = chainWork(header.bits) + blockchain.tip.chainwork)
    blockchain.copy(headersMap = blockchain.headersMap + (blockIndex.hash -> blockIndex), bestchain = blockchain.bestchain :+ blockIndex)
  }

  @tailrec
  def addHeadersLoop(bc: Blockchain, height: Int, headers: Seq[BlockHeader] = Nil): Blockchain =
    if (headers.isEmpty) bc else addHeadersLoop(Blockchain.addHeader(bc, height, headers.head), height + 1, headers.tail)

  def addHeaders(blockchain: Blockchain, height: Int, headers: Seq[BlockHeader] = Nil): Blockchain =
    if (height % RETARGETING_PERIOD == 0) addHeadersChunk(blockchain, height, headers)
    else addHeadersLoop(blockchain, height, headers)

  def chainWork(target: BigInt): BigInt = BigInt(2).pow(256) / (BigInt(1) + target)

  def chainWork(bits: Long): BigInt = {
    val (target, negative, overflow) = decodeCompact(bits)
    if (target == BigInteger.ZERO || negative || overflow) BigInt(0)
    else chainWork(target)
  }

  @tailrec
  def optimize(blockchain: Blockchain, acc: BlockIdxVec = Vector.empty): (Blockchain, BlockIdxVec) =
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

  def getDifficulty(blockchain: Blockchain, height: Int, dataDb: SQLiteData): Option[Long] =
    if (!blockchain.enforceSameBits) None
    else if (height % RETARGETING_PERIOD == 0) {
      for {
        parent <- blockchain.getHeader(height - 1) orElse dataDb.getHeader(height - 1)
        previous <- blockchain.getHeader(height - 2016) orElse dataDb.getHeader(height - 2016)
      } yield BlockHeader.calculateNextWorkRequired(parent, previous.time)
    } else {
      val hot = blockchain.getHeader(height - 1)
      val cold = dataDb.getHeader(height - 1)
      hot.orElse(cold).map(_.bits)
    }
}
