package com.wavesplatform.history

import java.util.concurrent.locks.{ReentrantReadWriteLock => RWL}
import javax.sql.DataSource

import com.wavesplatform.database.SQLiteWriter
import com.wavesplatform.features.FeatureProvider
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state2.reader.StateReader
import com.wavesplatform.state2.{BlockchainUpdaterImpl, StateWriter}
import scorex.transaction._

import scala.util.Try

object StorageFactory {

  def apply(settings: WavesSettings, ds: DataSource): Try[(NgHistory with DebugNgHistory, FeatureProvider, StateWriter, StateReader, BlockchainUpdater, BlockchainDebugInfo)] = {
    val lock = new RWL(true)
    for {
      historyWriter <- HistoryWriterImpl(settings.blockchainSettings.blockchainFile, lock, settings.blockchainSettings.functionalitySettings, settings.featuresSettings)
      stateWriter = new SQLiteWriter(ds)
    } yield {
      val bcu = BlockchainUpdaterImpl(stateWriter, historyWriter, settings, settings.blockchainSettings.minimumInMemoryDiffSize, lock)
      val history: NgHistory with DebugNgHistory with FeatureProvider = bcu.historyReader
      (history, history, stateWriter, bcu.bestLiquidState, bcu, bcu)
    }
  }
}
