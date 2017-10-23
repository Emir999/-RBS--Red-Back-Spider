package com.wavesplatform

import java.io.File
import java.util.concurrent.locks.ReentrantReadWriteLock

import com.typesafe.config.ConfigFactory
import com.wavesplatform.database.{PostgresWriter, createDataSource}
import com.wavesplatform.history.HistoryWriterImpl
import com.wavesplatform.settings.{WavesSettings, loadConfig}
import com.wavesplatform.state2.diffs.BlockDiffer
import scorex.account.AddressScheme
import scorex.block.Block
import scorex.transaction.Signed
import scorex.utils.ScorexLogging

object ImportTool extends ScorexLogging {
  def main(args: Array[String]): Unit = {
    val settings = WavesSettings.fromConfig(loadConfig(ConfigFactory.parseFile(new File(args(0)))))

    AddressScheme.current = new AddressScheme {
      override val chainId: Byte = settings.blockchainSettings.addressSchemeCharacter.toByte
    }

    val config = ConfigFactory.parseString("url = \"jdbc:postgresql:waves\"")
    val state = new PostgresWriter(createDataSource(config))

    val history = HistoryWriterImpl(settings.blockchainSettings.blockchainFile, new ReentrantReadWriteLock(true),
      settings.blockchainSettings.functionalitySettings, settings.featuresSettings).get
    val historyHeight = history.height
    val persistedHeight = state.height
    println(s"config file: ${new File(args(0)).toPath.toAbsolutePath}")
    println(s"Blockchain height: $historyHeight, file: ${settings.blockchainSettings.blockchainFile}, persistedHeight: $persistedHeight")

    (persistedHeight + 1 to historyHeight).foldLeft(Option.empty[Block]) {
      case (prevBlock, height) =>
        if (height % 200 == 0) {
          log.debug(s"Imported $height blocks")
        }
        val blockBytes = history.blockBytes(height).get
        val block = Block.parseBytes(blockBytes).get
        require(Signed.validateSignatures(block).isRight, "invalid block signature")
        BlockDiffer.fromBlock(settings.blockchainSettings.functionalitySettings, history, state, prevBlock, block) match {
          case Right(diff) =>
            state.append(diff, block)
            Some(block)
          case Left(e) =>
            println(s"at height $height:")
            throw new Exception(String.valueOf(e))
        }
    }
  }
}
