package fr.acinq.eclair.blockchain.electrum

import fr.acinq.bitcoin.{BlockHeader, ByteVector32, decodeCompact}
import trading.tacticaladvantage.sqlite.SQLiteData

import java.math.BigInteger
import scala.annotation.tailrec


case class Blockchain(retargetPeriod: Int,
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
  val ECX_RETARGETING_PERIOD = 288
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
    val needsBits = height % RETARGETING_PERIOD == 0 && cpindex >= 0 && cpindex < blockchain.checkpoints.length
    if (needsBits) Some(blockchain.checkpoints(cpindex).nextBits) else None
  }

  private def expectedBits(blockchain: Blockchain, height: Int, parent: BlockIndex): Option[Long] =
    if (height % blockchain.retargetPeriod != 0) Some(parent.header.bits) else {
      checkpointNextBits(blockchain, height).orElse {
        val oneOff = if (blockchain.retargetPeriod == ECX_RETARGETING_PERIOD) 1 else 0
        val referenceHeight = height - blockchain.retargetPeriod - oneOff
        ancestorAt(parent, referenceHeight).map { reference =>
          BlockHeader.calculateNextWorkRequired(parent.header, reference.header.time, blockchain.retargetPeriod)
        }
      }
    }

  def validateHeadersChunk(blockchain: Blockchain, height: Int, headers: Seq[BlockHeader] = Nil): Unit = {
    if (headers.isEmpty) return

    require(height >= 0 && height % RETARGETING_PERIOD == 0, "e1")
    require(headers.length <= RETARGETING_PERIOD, "e2")

    headers.foreach { header =>
      require(BlockHeader.checkProofOfWork(header), "e3")
    }

    headers.zip(headers.tail).foreach { case (previous, current) =>
      require(current.hashPreviousBlock == previous.hash, "e4")
    }

    val cpindex = (height / RETARGETING_PERIOD) - 1
    if (cpindex >= 0 && cpindex < blockchain.checkpoints.length) {
      require(headers.head.hashPreviousBlock == blockchain.checkpoints(cpindex).hash, "e5")
    }

    if (cpindex + 1 < blockchain.checkpoints.length) {
      require(headers.length == RETARGETING_PERIOD, "e16")
      require(headers.last.hash == blockchain.checkpoints(cpindex + 1).hash, "e7")
      return
    }

    val parent = blockchain.headersMap.get(headers.head.hashPreviousBlock)
    parent.foreach(blockIdx => require(blockIdx.height == height - 1, "e8"))

    val firstBits = checkpointNextBits(blockchain, height) orElse {
      parent.flatMap(blockIdx => expectedBits(blockchain, height, blockIdx))
    } getOrElse {
      throw new IllegalArgumentException(s"e9 $height")
    }

    var previous = BlockIndex(headers.head, height, parent)
    require(headers.head.bits == firstBits, s"e10 $height")
    headers.tail.zipWithIndex.foreach { case (current, offset) =>
        val currentHeight = height + offset + 1

        val bits = expectedBits(blockchain, currentHeight, previous).getOrElse {
          throw new IllegalArgumentException(s"e11 $currentHeight")
        }

        require(current.bits == bits, s"e12 $currentHeight")
        previous = BlockIndex(current, currentHeight, Some(previous))
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
        val blockIndex = BlockIndex(headers.head, height, blockchain.headersMap.get(headers.head.hashPreviousBlock),
          chainWork(headers.head.bits) + chainwork.sum)

        val bestchain1 = doAddHeaders(Vector(blockIndex), headers)
        val headersMap1 = blockchain.headersMap ++ bestchain1.map(bi => bi.hash -> bi)
        blockchain.copy(bestchain = bestchain1, headersMap = headersMap1)

      case _ if height < blockchain.checkpoints.length * RETARGETING_PERIOD =>
        blockchain

      case _ if height == blockchain.height + 1 =>
        require(headers.head.hashPreviousBlock == blockchain.bestchain.last.hash, "e13")
        val cumulative = chainWork(headers.head.bits) + blockchain.bestchain.last.chainwork
        val blockIndex = BlockIndex(headers.head, height, Some(blockchain.tip), cumulative)

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

    val expected = expectedBits(blockchain, height, blockchain.tip)
    require(expected.getOrElse(throw new IllegalArgumentException) == header.bits, "e17")

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
      val anchor = saveme.last.copy(parent = None)

      val rebuilt = blockchain.bestchain.drop(RETARGETING_PERIOD).foldLeft(Vector(anchor)) {
        case (chain, index) => chain :+ index.copy(parent = chain.lastOption)
      }

      val headersMap1 = rebuilt.map(index => index.hash -> index).toMap
      val checkpoints1 = blockchain.checkpoints :+ CheckPoint(anchor.hash, rebuilt.tail.head.header.bits)
      optimize(blockchain.copy(bestchain = rebuilt.tail, headersMap = headersMap1, checkpoints = checkpoints1), acc ++ saveme)
    } else {
      val safeWithoutCheckpoint = blockchain.bestchain.dropRight(MAX_REORG)
      (blockchain, acc ++ safeWithoutCheckpoint)
    }

  def getDifficulty(blockchain: Blockchain, height: Int, dataDb: SQLiteData): Option[Long] = {
    def headerAt(header: Int): Option[BlockHeader] = blockchain.getHeader(header).orElse(dataDb getHeader header)

    if (height % blockchain.retargetPeriod != 0) headerAt(height - 1).map(_.bits) else {
      checkpointNextBits(blockchain, height).orElse {
        val oneOff = if (blockchain.retargetPeriod == ECX_RETARGETING_PERIOD) 1 else 0
        val referenceHeight = height - blockchain.retargetPeriod - oneOff

        for {
          parent <- headerAt(height - 1)
          reference <- headerAt(referenceHeight)
        } yield BlockHeader.calculateNextWorkRequired(parent, reference.time, blockchain.retargetPeriod)
      }
    }
  }
}
