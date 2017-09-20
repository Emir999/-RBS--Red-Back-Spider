package com.wavesplatform.state2.patch

import com.wavesplatform.state2.reader.StateReader
import com.wavesplatform.state2.{Diff, LeaseInfo, Portfolio}

object LeasePatch {
  def apply(s: StateReader): Diff = {

    def invertLeaseInfo(l: LeaseInfo): LeaseInfo = LeaseInfo(-l.leaseIn, -l.leaseOut )

    val portfolioUpd = s.nonZeroLeaseBalances
      .collect { case (acc, pf) if pf != LeaseInfo.empty =>
        acc -> Portfolio(0, invertLeaseInfo(pf), Map.empty)
      }

    Diff(transactions = Map.empty,
      portfolios = portfolioUpd,
      issuedAssets = Map.empty,
      aliases = Map.empty,
      paymentTransactionIdsByHashes = Map.empty,
      orderFills = Map.empty,
      leaseState = s.activeLeases.map(_ -> false).toMap)
  }
}
