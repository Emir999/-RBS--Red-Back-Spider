package com.wavesplatform.state2

case class WavesBalance(balance: Long = 0, leaseInfo: LeaseInfo = LeaseInfo.empty) {
  lazy val effectiveBalance: Long = safeSum(balance, leaseInfo.leaseIn) - leaseInfo.leaseOut
}
