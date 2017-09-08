package com.wavesplatform

import java.io.File
import java.util.Properties
import java.util.concurrent.locks.ReentrantReadWriteLock

import com.typesafe.config.ConfigFactory
import com.wavesplatform.history.HistoryWriterImpl
import com.wavesplatform.settings.{WavesSettings, loadConfig}
import com.wavesplatform.state2.StateWriterImpl
import com.wavesplatform.state2.diffs.BlockDiffer
import com.zaxxer.hikari.{HikariConfig, HikariDataSource}
import org.flywaydb.core.Flyway
import scorex.account.AddressScheme
import scorex.block.Block
import scorex.utils.ScorexLogging

object ImportTool extends ScorexLogging {
  def main(args: Array[String]): Unit = {
    val settings = WavesSettings.fromConfig(loadConfig(ConfigFactory.parseFile(new File(args(0)))))

    AddressScheme.current = new AddressScheme {
      override val chainId: Byte = settings.blockchainSettings.addressSchemeCharacter.toByte
    }

    val props = new Properties()
    props.put("url", s"jdbc:h2:${settings.directory}/h2db")
    val hc = new HikariConfig()
    hc.setDataSourceClassName("org.h2.jdbcx.JdbcDataSource")
    //    hc.setDriverClassName("org.h2.Driver")
    hc.setUsername("sa")
    hc.setPassword("sa")
    hc.setDataSourceProperties(props)
    val hds = new HikariDataSource(hc)
    val flyway = new Flyway
    flyway.setDataSource(hds)
    flyway.migrate()

    val state = new StateWriterImpl(hds)

    val history = HistoryWriterImpl(settings.blockchainSettings.blockchainFile, new ReentrantReadWriteLock(true),
      settings.blockchainSettings.functionalitySettings, settings.featuresSettings).get
    val historyHeight = history.height()
    val persistedHeight = state.height
    println(s"${settings.blockchainSettings}")
    println(s"config file: ${new File(args(0)).toPath.toAbsolutePath}")
    println(s"Blockchain height: $historyHeight, file: ${settings.blockchainSettings.blockchainFile}, persistedHeight: $persistedHeight")



    (persistedHeight + 1 to 600000).foldLeft(Option.empty[Block]) {
      case (prevBlock, height) =>
        if (height % 200 == 0) {
          log.debug(s"Imported $height blocks")
        }
        val blockBytes = history.blockBytes(height).get
        val block = Block.parseBytes(blockBytes).toOption
        BlockDiffer.fromBlock(settings.blockchainSettings.functionalitySettings, history, state, prevBlock, block.get) match {
          case Right(diff) =>
            state.applyBlockDiff(diff, blockBytes, height)
            block
          case Left(e) =>
            println(e)
            throw new Exception(String.valueOf(e))
        }
    }

    1 to 10 foreach { height =>
      val block = Block.parseBytes(history.blockBytes(height).get)

    }
  }
}
