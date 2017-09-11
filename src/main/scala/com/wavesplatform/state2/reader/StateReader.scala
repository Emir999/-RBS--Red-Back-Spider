package com.wavesplatform.state2.reader

import com.wavesplatform.state2._
import scorex.account.{Address, AddressOrAlias, Alias}
import scorex.transaction.ValidationError.AliasNotExists
import scorex.transaction._
import scorex.transaction.assets.IssueTransaction
import scorex.utils.ScorexLogging

import scala.util.{Right, Try}

trait StateReader {

  def accountPortfolios: Map[Address, Portfolio]

  def transactionInfo(id: ByteStr): Option[(Int, Transaction)]

  def containsTransaction(id: ByteStr): Boolean

  def accountPortfolio(a: Address): Portfolio

  def assetDescription(id: ByteStr): Option[AssetDescription]

  def assetInfo(id: ByteStr): Option[AssetInfo]

  def wavesBalance(a: Address): WavesBalance

  def assetBalance(a: Address, asset: ByteStr): Long

  def height: Int

  def accountTransactionIds(a: Address, limit: Int): Seq[ByteStr]

  def paymentTransactionIdByHash(hash: ByteStr): Option[ByteStr]

  def aliasesOfAddress(a: Address): Seq[Alias]

  def resolveAlias(a: Alias): Option[Address]

  def leaseInfo(leaseId: ByteStr): Option[LeaseInfo]

  def activeLeases(): Seq[ByteStr]

  def lastUpdateHeight(acc: Address): Option[Int]

  def snapshotAtHeight(acc: Address, h: Int): Option[Snapshot]

  def filledVolumeAndFee(orderId: ByteStr): OrderFillInfo
}

object StateReader {

  implicit class StateReaderExt(s: StateReader) extends ScorexLogging {
    def assetDistribution(assetId: ByteStr): Map[Address, Long] = ???

    def resolveAliasEi[T <: Transaction](aoa: AddressOrAlias): Either[ValidationError, Address] = {
      aoa match {
        case a: Address => Right(a)
        case a: Alias => s.resolveAlias(a) match {
          case None => Left(AliasNotExists(a))
          case Some(acc) => Right(acc)
        }
      }
    }

    def included(signature: ByteStr): Option[Int] = s.transactionInfo(signature).map(_._1)

    def accountTransactions(account: Address, limit: Int): Seq[_ <: Transaction] = Seq.empty

    def getAccountBalance(account: Address): Map[AssetId, (Long, Boolean, Long, IssueTransaction)] = Map.empty

    def assetDistribution(assetId: Array[Byte]): Map[String, Long] =
      s.assetDistribution(ByteStr(assetId))
        .map { case (acc, amt) => (acc.address, amt) }

    def isReissuable(id: Array[Byte]): Boolean =
      s.assetInfo(ByteStr(id)).get.isReissuable

    def totalAssetQuantity(assetId: AssetId): Long =
      s.assetInfo(assetId).get.volume

    private def minBySnapshot(acc: Address, atHeight: Int, confirmations: Int)(extractor: Snapshot => Long): Long = ???

    def effectiveBalanceAtHeightWithConfirmations(acc: Address, atHeight: Int, confirmations: Int): Try[Long] = Try {
      minBySnapshot(acc, atHeight, confirmations)(_.effectiveBalance)
    }

    def balanceWithConfirmations(acc: Address, confirmations: Int): Long =
      minBySnapshot(acc, s.height, confirmations)(_.balance)

    def balanceAtHeight(acc: Address, height: Int): Long = ???

    def accountPortfoliosHash: Int = {
      Hash.accountPortfolios(s.accountPortfolios)
    }

    def partialPortfolio(a: Address, assets: Set[AssetId] = Set.empty): Portfolio = {
      val w = s.wavesBalance(a)
      Portfolio(w.balance, w.leaseInfo, assets.map(id => id -> s.assetBalance(a, id)).toMap)
    }
  }

}
