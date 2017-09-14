package com.wavesplatform.state2.reader

import cats.implicits._
import cats.kernel.Monoid
import com.wavesplatform.state2._
import scorex.account.{Address, Alias}
import scorex.transaction.Transaction
import scorex.transaction.assets.IssueTransaction
import scorex.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}

class CompositeStateReader(inner: StateReader, blockDiff: BlockDiff) extends StateReader {

  private val txDiff = blockDiff.txsDiff

  override def assetDescription(id: ByteStr) = {
    val issueInThisBlock = txDiff.transactions.values.collectFirst {
      case (_, tx: IssueTransaction, _) if tx.id == id =>
        val ai = blockDiff.txsDiff.issuedAssets.getOrElse(tx.id, AssetInfo(tx.reissuable, tx.quantity))
        AssetDescription(tx.sender, tx.name, tx.description, tx.decimals, ai)
    }

    val reissueInThisBlock = txDiff.issuedAssets.getOrElse(id, Monoid.empty[AssetInfo])

    issueInThisBlock orElse inner.assetDescription(id).map(d => d.copy(info = d.info.combine(reissueInThisBlock)))
  }

  override def nonZeroLeaseBalances = inner.nonZeroLeaseBalances.combine(blockDiff.txsDiff.portfolios.map {
    case (addr, p) => addr -> p.leaseInfo
  })

  override def leaseInfo(a: Address) = {
    inner.leaseInfo(a).combine(blockDiff.txsDiff.portfolios.get(a).fold(LeaseInfo.empty)(_.leaseInfo))
  }

  override def leaseDetails(leaseId: ByteStr) = {
    val leaseInThisBlock = blockDiff.txsDiff.transactions.collectFirst {
      case (`leaseId`, (_, lt: LeaseTransaction, _)) =>
        LeaseDetails(lt.sender, lt.recipient, inner.height + blockDiff.heightDiff, lt.amount, true)
    }

    leaseInThisBlock.orElse(inner.leaseDetails(leaseId)).map { ld =>
      if (blockDiff.txsDiff.transactions.exists {
        case (_, (_, lc: LeaseCancelTransaction, _)) => lc.leaseId == leaseId
        case _ => false
      }) ld.copy(isActive = false) else ld
    }
  }

  override def transactionInfo(id: ByteStr): Option[(Int, Transaction)] =
    txDiff.transactions.get(id)
      .map(t => (t._1, t._2))
      .orElse(inner.transactionInfo(id))

  override def accountPortfolio(a: Address): Portfolio =
    inner.accountPortfolio(a).combine(txDiff.portfolios.get(a).orEmpty)

  override def assetInfo(id: ByteStr): Option[AssetInfo] = (inner.assetInfo(id), txDiff.issuedAssets.get(id)) match {
    case (None, None) => None
    case (existing, upd) => Some(existing.orEmpty.combine(upd.orEmpty))
  }

  override def height: Int = inner.height + blockDiff.heightDiff

  override def accountTransactionIds(a: Address, limit: Int): Seq[ByteStr] = {
    val fromDiff = txDiff.accountTransactionIds.get(a).orEmpty
    if (fromDiff.length >= limit) {
      fromDiff.take(limit)
    } else {
      fromDiff ++ inner.accountTransactionIds(a, limit - fromDiff.size) // fresh head ++ stale tail
    }
  }

  override def wavesBalance(a: Address) = {
    val i = inner.wavesBalance(a)
    WavesBalance(i.regularBalance + txDiff.portfolios.get(a).fold(0L)(_.balance), i.effectiveBalance)
  }

  override def assetBalance(a: Address) = ???

  override def snapshotAtHeight(acc: Address, h: Int): Option[Snapshot] = ???

  override def paymentTransactionIdByHash(hash: ByteStr): Option[ByteStr]
  = blockDiff.txsDiff.paymentTransactionIdsByHashes.get(hash)
    .orElse(inner.paymentTransactionIdByHash(hash))


  override def aliasesOfAddress(a: Address): Seq[Alias] =
    txDiff.aliases.filter(_._2 == a).keys.toSeq ++ inner.aliasesOfAddress(a)

  override def resolveAlias(a: Alias): Option[Address] = txDiff.aliases.get(a).orElse(inner.resolveAlias(a))

  override def activeLeases: Seq[ByteStr] = {
    blockDiff.txsDiff.leaseState.collect { case (id, isActive) if isActive => id }.toSeq ++ inner.activeLeases
  }

  override def lastUpdateHeight(acc: Address): Option[Int] = ???

  override def containsTransaction(id: ByteStr): Boolean = blockDiff.txsDiff.transactions.contains(id) || inner.containsTransaction(id)

  override def filledVolumeAndFee(orderId: ByteStr): OrderFillInfo =
    blockDiff.txsDiff.orderFills.get(orderId).orEmpty.combine(inner.filledVolumeAndFee(orderId))
}

object CompositeStateReader {

  class Proxy(val inner: StateReader, blockDiff: () => BlockDiff) extends StateReader {
    override def accountPortfolio(a: Address): Portfolio = ???

    override def leaseInfo(a: Address): LeaseInfo = ???

    override def assetDescription(id: ByteStr): Option[AssetDescription] = ???

    override def leaseDetails(leaseId: ByteStr): Option[LeaseDetails] = ???

    override def wavesBalance(a: Address): WavesBalance = ???

    override def assetBalance(a: Address): Map[ByteStr, Long] = ???

    override def paymentTransactionIdByHash(hash: ByteStr): Option[ByteStr] =
      new CompositeStateReader(inner, blockDiff()).paymentTransactionIdByHash(hash)

    override def aliasesOfAddress(a: Address): Seq[Alias] =
      new CompositeStateReader(inner, blockDiff()).aliasesOfAddress(a)

    override def accountTransactionIds(a: Address, limit: Int): Seq[ByteStr] =
      new CompositeStateReader(inner, blockDiff()).accountTransactionIds(a, limit)

    override def transactionInfo(id: ByteStr): Option[(Int, Transaction)] =
      new CompositeStateReader(inner, blockDiff()).transactionInfo(id)

    override def resolveAlias(a: Alias): Option[Address] =
      new CompositeStateReader(inner, blockDiff()).resolveAlias(a)

    override def assetInfo(id: ByteStr): Option[AssetInfo] =
      new CompositeStateReader(inner, blockDiff()).assetInfo(id)

    override def height: Int =
      new CompositeStateReader(inner, blockDiff()).height

    override def nonZeroLeaseBalances: Map[Address, LeaseInfo] =
      new CompositeStateReader(inner, blockDiff()).nonZeroLeaseBalances

    override def activeLeases: Seq[ByteStr] =
      new CompositeStateReader(inner, blockDiff()).activeLeases

    override def lastUpdateHeight(acc: Address): Option[Int] =
      new CompositeStateReader(inner, blockDiff()).lastUpdateHeight(acc)

    override def snapshotAtHeight(acc: Address, h: Int): Option[Snapshot] =
      new CompositeStateReader(inner, blockDiff()).snapshotAtHeight(acc, h)

    override def containsTransaction(id: ByteStr): Boolean =
      new CompositeStateReader(inner, blockDiff()).containsTransaction(id)

    override def filledVolumeAndFee(orderId: ByteStr): OrderFillInfo =
      new CompositeStateReader(inner, blockDiff()).filledVolumeAndFee(orderId)
  }

  def composite(inner: StateReader, blockDiff: () => BlockDiff): Proxy = new Proxy(inner, blockDiff)
}
