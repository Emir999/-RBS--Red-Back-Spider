package scorex.transaction

import com.wavesplatform.network.{BlockCheckpoint, Checkpoint}
import com.wavesplatform.state2.ByteStr
import scorex.block.Block.BlockId
import scorex.block.{Block, MicroBlock}
import scorex.consensus.nxt.NxtLikeConsensusBlockData
import scorex.transaction.History.{BlockMinerInfo, BlockchainScore}

import scala.util.Try

trait History {
  def height: Int

  def score: BlockchainScore

  def lastBlock: Option[Block]

  def blockBytes(height: Int): Option[Array[Byte]]

  def blockBytes(blockId: ByteStr): Option[Array[Byte]]

  def scoreOf(id: ByteStr): Option[BlockchainScore]

  def heightOf(blockId: ByteStr): Option[Int]

  def lastBlockIds(howMany: Int): Seq[ByteStr]

  def blockIdsAfter(parentSignature: ByteStr, howMany: Int): Seq[ByteStr]

  def parent(ofBlock: Block, back: Int): Option[Block]
}

trait NgHistory extends History {
  def microBlock(id: ByteStr): Option[MicroBlock]

  def bestLastBlockInfo(maxTimestamp: Long): Option[BlockMinerInfo]
}

trait DebugNgHistory {
  def lastPersistedBlockIds(count: Int): Seq[BlockId]

  def microblockIds(): Seq[BlockId]
}

trait CheckpointService {

  def set(checkpoint: Checkpoint): Either[ValidationError, Unit]

  def get: Option[Checkpoint]
}

object CheckpointService {

  implicit class CheckpointServiceExt(cs: CheckpointService) {
    def isBlockValid(candidateSignature: ByteStr, estimatedHeight: Int): Boolean =
      !cs.get.exists {
        _.items.exists { case BlockCheckpoint(h, sig) =>
          h == estimatedHeight && candidateSignature != ByteStr(sig)
        }
      }
  }

}

object History {

  type BlockchainScore = BigInt

  case class BlockMinerInfo(consensus: NxtLikeConsensusBlockData, timestamp: Long, blockId: BlockId)

  implicit class HistoryExt(history: History) {

    def score: BlockchainScore = {
      history.lastBlock.flatMap(last => history.scoreOf(last.uniqueId)).getOrElse(0)
    }

    def isEmpty: Boolean = history.height == 0

    def contains(block: Block): Boolean = history.contains(block.uniqueId)

    def contains(signature: ByteStr): Boolean = history.heightOf(signature).isDefined

    def blockById(blockId: ByteStr): Option[Block] = {
      history.heightOf(blockId).flatMap(history.blockAt)
    }

    def blockAt(height: Int): Option[Block] = history.blockBytes(height).flatMap(bb => Block.parseBytes(bb).toOption)

    def averageDelay(block: Block, blockNum: Int): Try[Long] = Try {
      (block.timestamp - parent(block, blockNum).get.timestamp) / blockNum
    }

    def parent(block: Block, back: Int = 1): Option[Block] = {
      require(back > 0)
      history.heightOf(block.reference).flatMap(referenceHeight => history.blockAt(referenceHeight - back + 1))
    }

    def lastBlocks(howMany: Int): Seq[Block] = {
      (Math.max(1, history.height - howMany + 1) to history.height).flatMap(history.blockAt).reverse
    }

    def genesis: Block = history.blockAt(1).get

    def lastBlockTimestamp = history.lastBlock.map(_.timestamp)
    def lastBlockId = history.lastBlock.map(_.uniqueId)
  }
}
