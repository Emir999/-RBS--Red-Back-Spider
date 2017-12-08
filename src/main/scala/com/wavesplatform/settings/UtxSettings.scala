package com.wavesplatform.settings

import java.util.Map

import com.typesafe.config.{Config, ConfigValue}
import scorex.transaction.{SignedTransaction}
import sun.tools.jconsole.inspector.Utils

import scala.concurrent.duration.FiniteDuration

case class UtxSettings(maxSize: Int,
                       maxTransactionAge: FiniteDuration,
                       blacklistSenderAddresses: Set[String],
                       allowBlacklistedTransferTo: Set[String],
                       cleanupInterval: FiniteDuration)

case class Bar(appliesWhen: AppliesWhen)

trait AppliesWhen {
  def check(signedTransaction: SignedTransaction): Boolean
}

