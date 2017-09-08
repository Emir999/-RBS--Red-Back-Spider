package com.wavesplatform.state2.reader

import cats.implicits._
import com.wavesplatform.state2._
import scorex.account.{Address, Alias}
import scorex.transaction.lease.LeaseTransaction
import scorex.transaction.{Transaction, TransactionParser}

import scala.collection.JavaConverters._

class StateReaderImpl(p: StateStorage) extends StateReader {

  override def transactionInfo(id: ByteStr): Option[(Int, Transaction)] = {
    Option(p.transactions.get(id)).map {
      case (h, bytes) => (h, TransactionParser.parseBytes(bytes).get)
    }
  }

  override def accountPortfolio(a: Address): Portfolio = ???

  override def assetInfo(id: ByteStr): Option[AssetInfo] = ???

  override def height: Int = { p.getHeight }

  override def accountTransactionIds(a: Address, limit: Int): Seq[ByteStr] = {
    val totalRecords = p.accountTransactionsLengths.getOrDefault(a.bytes, 0)
    Range(Math.max(0, totalRecords - limit), totalRecords)
      .map(n => p.accountTransactionIds.get(StateStorage.accountIndexKey(a, n)))
      .reverse
  }

  override def paymentTransactionIdByHash(hash: ByteStr): Option[ByteStr] = {
    Option(p.paymentTransactionHashes.get(hash))
  }

  override def aliasesOfAddress(a: Address): Seq[Alias] = {
    p.aliasToAddress.asScala
      .collect { case (aliasName, addressBytes) if addressBytes == a.bytes =>
        Alias.buildWithCurrentNetworkByte(aliasName).explicitGet()
      }.toSeq
  }

  override def resolveAlias(a: Alias): Option[Address] = {
    Option(p.aliasToAddress.get(a.name))
      .map(b => Address.fromBytes(b.arr).explicitGet())
  }

  override def accountPortfolios: Map[Address, Portfolio] = ???

  override def isLeaseActive(leaseTx: LeaseTransaction): Boolean = {
    p.leaseState.getOrDefault(leaseTx.id, false)
  }

  override def activeLeases(): Seq[ByteStr] = {
    p.leaseState
      .asScala
      .collect { case (leaseId, isActive) if isActive => leaseId }
      .toSeq
  }

  override def lastUpdateHeight(acc: Address): Option[Int] =  {
    Option(p.lastBalanceSnapshotHeight.get(acc.bytes))
  }

  override def snapshotAtHeight(acc: Address, h: Int): Option[Snapshot] =  {
    Option(p.balanceSnapshots.get(StateStorage.accountIndexKey(acc, h)))
      .map { case (ph, b, eb) => Snapshot(ph, b, eb) }
  }

  override def containsTransaction(id: ByteStr): Boolean = {
    p.transactions.containsKey(id)
  }

  override def filledVolumeAndFee(orderId: ByteStr): OrderFillInfo = {
    Option(p.orderFills.get(orderId)).map(oi => OrderFillInfo(oi._1, oi._2)).orEmpty
  }

  override def wavesBalance(a: Address) = ???

  override def assetBalance(a: Address, asset: ByteStr) = ???
}
