package com.wavesplatform.history

import java.io.File
import java.util.concurrent.locks.ReentrantReadWriteLock

import com.wavesplatform.features.FeatureProvider
import com.wavesplatform.settings.{FeaturesSettings, FunctionalitySettings}
import com.wavesplatform.state2.{BlockDiff, ByteStr, DataTypes, VariablesStorage, VersionableStorage}
import com.wavesplatform.utils._
import kamon.Kamon
import scorex.block.Block
import scorex.transaction.History.BlockchainScore
import scorex.transaction.ValidationError.GenericError
import scorex.transaction._
import scorex.utils.{LogMVMapBuilder, ScorexLogging}

import scala.collection.JavaConverters._
import scala.util.Try

class HistoryWriterImpl private(file: Option[File], val synchronizationToken: ReentrantReadWriteLock,
                                functionalitySettings: FunctionalitySettings, featuresSettings: FeaturesSettings)
  extends VariablesStorage(createMVStore(file)) with VersionableStorage with History with AutoCloseable with FeatureProvider with ScorexLogging {

  override protected val Version: Int = 1

  import HistoryWriterImpl._

  override def close() = ???

  override val activationWindowSize: Int = functionalitySettings.featureCheckBlocksPeriod
  val MinVotesWithinWindowToActivateFeature: Int = functionalitySettings.blocksForFeatureActivation

  private val blockBodyByHeight = db.openMap("blocks", new LogMVMapBuilder[Int, Array[Byte]])
  private val blockIdByHeight = db.openMap("signatures", new LogMVMapBuilder[Int, ByteStr].valueType(DataTypes.byteStr))
  private val heightByBlockId = db.openMap("signaturesReverse", new LogMVMapBuilder[ByteStr, Int].keyType(DataTypes.byteStr))
  private val scoreByHeight = db.openMap("score", new LogMVMapBuilder[Int, BigInt])
  private val featuresVotes = db.openMap("features-votes", new LogMVMapBuilder[Int, Map[Short, Int]].valueType(DataTypes.mapShortInt))
  private val featuresState = db.openMap("features-state", new LogMVMapBuilder[Short, Int])

  private[HistoryWriterImpl] def isConsistent: Boolean = {
    // check if all maps have same size
    Set(blockBodyByHeight.size(), blockIdByHeight.size(), heightByBlockId.size(), scoreByHeight.size()).size == 1
  }

  private lazy val preAcceptedFeatures = functionalitySettings.preActivatedFeatures.mapValues(h => h - activationWindowSize)

  override def approvedFeatures(): Map[Short, Int] = {
    preAcceptedFeatures ++ featuresState.asScala
  }

  override def featureVotesCountWithinActivationWindow(height: Int): Map[Short, Int] = {
    featuresVotes.getOrDefault(FeatureProvider.votingWindowOpeningFromHeight(height, activationWindowSize), Map.empty)
  }

  private def alterVotes(height: Int, votes: Set[Short], voteMod: Int): Unit = {
    val votingWindowOpening = FeatureProvider.votingWindowOpeningFromHeight(height, activationWindowSize)
    val votesWithinWindow = featuresVotes.getOrDefault(votingWindowOpening, Map.empty[Short, Int])
    val newVotes = votes.foldLeft(votesWithinWindow)((v, feature) => v + (feature -> (v.getOrElse(feature, 0) + voteMod)))
    featuresVotes.put(votingWindowOpening, newVotes)
  }

  def appendBlock(block: Block, acceptedFeatures: Set[Short])(consensusValidation: => Either[ValidationError, BlockDiff]): Either[ValidationError, BlockDiff] =
    {

      assert(block.signatureValid)

      if ((height == 0) || (this.lastBlock.get.uniqueId == block.reference)) consensusValidation.map { blockDiff =>
        val h = height + 1
        val score = (if (height == 0) BigInt(0) else this.score) + block.blockScore
        blockBodyByHeight.put(h, block.bytes)
        scoreByHeight.put(h, score)
        blockIdByHeight.put(h, block.uniqueId)
        heightByBlockId.put(block.uniqueId, h)
        featuresState.putAll(acceptedFeatures.diff(featuresState.keySet.asScala).map(_ -> h).toMap.asJava)
        alterVotes(h, block.featureVotes, 1)
        db.commit()
        blockHeightStats.record(h)
        blockSizeStats.record(block.bytes.length)
        transactionsInBlockStats.record(block.transactionData.size)

        if (h % 100 == 0) db.compact(CompactFillRate, CompactMemorySize)

        log.trace(s"Full Block $block(id=${block.uniqueId} persisted")
        blockDiff
      }
      else {
        Left(GenericError(s"Parent ${block.reference} of block ${block.uniqueId} does not match last block ${this.lastBlock.map(_.uniqueId)}"))
      }
    }

  def discardBlock(): Option[Block] = {
    val h = height

    alterVotes(h, this.blockAt(h).map(b => b.featureVotes).getOrElse(Set.empty), -1)

    val removedBlockBytes = blockBodyByHeight.remove(h)
    val maybeDiscardedBlock = Block.parseBytes(removedBlockBytes).toOption
    scoreByHeight.remove(h)

    if (h % activationWindowSize == 0) {
      val featuresToRemove = featuresState.entrySet().asScala.filter(_.getValue == h).map(_.getKey)
      featuresToRemove.foreach(featuresState.remove)
    }

    val vOpt = Option(blockIdByHeight.remove(h))
    vOpt.map(v => heightByBlockId.remove(v))
    db.commit()

    maybeDiscardedBlock
  }

  override def lastBlockIds(howMany: Int): Seq[ByteStr] = {
    (Math.max(1, height - howMany + 1) to height).flatMap(i => Option(blockIdByHeight.get(i)))
      .reverse
  }

  override def height: Int = blockIdByHeight.size()

  override def scoreOf(id: ByteStr): Option[BlockchainScore] = {
    heightOf(id).map(scoreByHeight.get(_))
  }

  override def heightOf(blockSignature: ByteStr): Option[Int] = {
    Option(heightByBlockId.get(blockSignature))
  }

  override def blockBytes(height: Int): Option[Array[Byte]] = {
    Option(blockBodyByHeight.get(height))
  }

  override def score = ???

  override def lastBlock: Option[Block] = ???

  override def blockBytes(blockId: AssetId) = ???

  override def blockIdsAfter(parentSignature: AssetId, howMany: Int) = ???

  override def parent(ofBlock: Block, back: Int) = ???
}

object HistoryWriterImpl extends ScorexLogging {
  private val CompactFillRate = 90
  private val CompactMemorySize = 10 * 1024 * 1024

  def apply(file: Option[File], synchronizationToken: ReentrantReadWriteLock, functionalitySettings: FunctionalitySettings,
            featuresSettings: FeaturesSettings): Try[HistoryWriterImpl] =
    createWithStore[HistoryWriterImpl](file, new HistoryWriterImpl(file, synchronizationToken, functionalitySettings, featuresSettings), h => h.isConsistent)

  private val blockHeightStats = Kamon.metrics.histogram("block-height")
  private val blockSizeStats = Kamon.metrics.histogram("block-size-bytes")
  private val transactionsInBlockStats = Kamon.metrics.histogram("transactions-in-block")
}
