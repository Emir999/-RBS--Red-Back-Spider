package scorex.transaction

import cats.implicits._
import com.wavesplatform.features.FeatureProvider
import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state2._
import scorex.block.Block.BlockId
import scorex.block.{Block, MicroBlock}
import scorex.transaction.History.{BlockMinerInfo, BlockchainScore}

class NgHistoryReader(ngState: () => Option[NgState], inner: History with FeatureProvider, settings: FunctionalitySettings) extends History with NgHistory with DebugNgHistory with FeatureProvider {

  override val activationWindowSize: Int = settings.featureCheckBlocksPeriod

  override def height: Int = {
    inner.height + ngState().map(_ => 1).getOrElse(0)
  }

  override def blockBytes(height: Int): Option[Array[Byte]] = {
    inner.blockBytes(height).orElse(if (height == inner.height + 1) ngState().map(_.bestLiquidBlock.bytes) else None)
  }

  override def scoreOf(blockId: BlockId): Option[BlockchainScore] = {
    inner.scoreOf(blockId)
      .orElse(ngState() match {
        case Some(ng) if ng.contains(blockId) => Some(inner.score + ng.base.blockScore)
        case _ => None
      })
  }

  override def heightOf(blockId: BlockId): Option[Int] = {
    lazy val innerHeight = inner.height
    inner.heightOf(blockId).orElse(ngState() match {
      case Some(ng) if ng.contains(blockId) => Some(innerHeight + 1)
      case _ => None
    })
  }

  override def lastBlockIds(howMany: Int): Seq[BlockId] = {
    ngState() match {
      case Some(ng) =>
        ng.bestLiquidBlockId +: inner.lastBlockIds(howMany - 1)
      case None =>
        inner.lastBlockIds(howMany)
    }
  }

  override def microBlock(id: BlockId): Option[MicroBlock] = {
    for {
      ng <- ngState()
      mb <- ng.micros.find(_.totalResBlockSig == id)
    } yield mb
  }

  def lastBlockTimestamp: Option[Long] = {
    ngState().map(_.base.timestamp).orElse(inner.lastBlockTimestamp)
  }

  def lastBlockId(): Option[AssetId] = {
    ngState().map(_.bestLiquidBlockId).orElse(inner.lastBlock.map(_.uniqueId))
  }

  def blockAt(height: Int): Option[Block] = {
    if (height == inner.height + 1)
      ngState().map(_.bestLiquidBlock)
    else
      inner.blockAt(height)
  }

  override def lastPersistedBlockIds(count: Int): Seq[BlockId] = {
    inner.lastBlockIds(count)
  }

  override def microblockIds(): Seq[BlockId] = {
    ngState().toSeq.flatMap(ng => ng.micros.map(_.totalResBlockSig))
  }

  override def bestLastBlockInfo(maxTimestamp: Long): Option[BlockMinerInfo] = {
    ngState().map(_.bestLastBlockInfo(maxTimestamp))
      .orElse(inner.lastBlock.map(b => BlockMinerInfo(b.consensusData, b.timestamp, b.uniqueId)))
  }

  override def approvedFeatures(): Map[Short, Int] = {
    lazy val h = height
    ngState().map(_.acceptedFeatures.map(_ -> h).toMap).getOrElse(Map.empty) ++ inner.approvedFeatures()
  }

  override def featureVotesCountWithinActivationWindow(height: Int): Map[Short, Int] = {
    val ngVotes = ngState().map(_.base.featureVotes.map(_ -> 1).toMap).getOrElse(Map.empty)
    inner.featureVotesCountWithinActivationWindow(height) |+| ngVotes
  }

  override def score = ???

  override def lastBlock = ???

  override def blockBytes(blockId: AssetId) = ???

  override def blockIdsAfter(parentSignature: AssetId, howMany: Int) = ???

  override def parent(ofBlock: Block, back: Int) = ???
}
