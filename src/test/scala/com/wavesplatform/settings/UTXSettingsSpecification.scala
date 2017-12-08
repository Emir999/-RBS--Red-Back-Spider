package com.wavesplatform.settings

import com.typesafe.config.{Config, ConfigFactory, ConfigList, ConfigValue}
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._
import org.scalatest.{FlatSpec, Matchers}
import play.api.libs.functional.FunctionalCanBuild
import scorex.transaction.{SignedTransaction, TransactionParser}
import scorex.transaction.TransactionParser.TransactionType
import scorex.transaction.assets.TransferTransaction

import scala.concurrent.duration._
import scala.collection.JavaConverters._

trait TransactionConstraint{
  def canApply(tx: SignedTransaction): Boolean
  def check(tx: SignedTransaction): Boolean
}

object TransactionConstraint {
  def fromConfig(c: Config): TransactionConstraint = {

    val appliesWhen = c.getConfig("applies-when").entrySet().asScala.map(x => x.getKey -> x.getValue).map {
      case ("transaction-type", v) => (tx: SignedTransaction) =>
        TransactionParser.TransactionType.fromString(v.render) == tx.transactionType

      case ("fee-asset", v) => (tx: SignedTransaction) =>
        v.render() == tx.assetFee._1.getOrElse("WAVES")
    }

    new TransactionConstraint {
      override def canApply(tx: SignedTransaction): Boolean =
        appliesWhen.foldLeft(true)((result, check) => result & check(tx))

      override def check(tx: SignedTransaction): Boolean =
        true  //requirements.foldLeft(true)((result, check) => result & check(tx))
    }
  }
}

class UTXSettingsSpecification extends FlatSpec with Matchers {
  "UTXSettings" should "read values" in {
    val config = ConfigFactory.parseString(
      """waves {
        |  utx {
        |    max-size = 100
        |    max-transaction-age = 100m
        |    cleanup-interval = 10m
        |    blacklist-sender-addresses = ["a"]
        |    allow-blacklisted-transfer-to = ["b"]
        |    constraints = [
        |      {
        |        applies-when {
        |          transaction-type: Transfer
        |          fee-asset: "AAA"
        |        }
        |        requirements {
        |          minimal-fee: 0.001
        |          sender: ["A", "B"]
        |        }
        |      },
        |    ]
        |  }
        |}""".stripMargin).resolve()
    val settings = config.as[UtxSettings]("waves.utx")
    settings.maxSize should be(100)
    settings.maxTransactionAge shouldBe 100.minutes
    settings.cleanupInterval shouldBe 10.minutes
    settings.blacklistSenderAddresses shouldBe Set("a")
    settings.allowBlacklistedTransferTo shouldBe Set("b")

    val tc = config.getConfigList("waves.utx.constraints").asScala.map(c => c.as[Map[String, ConfigValue]]("requirements")
        .map {
          case ("minimal-fee", v: ConfigValue) =>
            val requiredFee = v.render.toLong
            (tx: SignedTransaction) => tx.assetFee._2 >= requiredFee
          case ("sender", list: ConfigList) =>
            val l = list.asScala.map(i => i.render)
            (tx: SignedTransaction) => l.contains(tx.sender.address)
        }
    )


    //var a = settings.constraints
    var c = tc
  }
}
