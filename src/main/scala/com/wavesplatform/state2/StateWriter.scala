package com.wavesplatform.state2

import java.sql.Timestamp
import javax.sql.DataSource

import com.google.common.cache.{CacheBuilder, CacheLoader}
import com.wavesplatform.state2.reader.{LeaseDetails, StateReader}
import scalikejdbc._
import scorex.account.{Address, AddressOrAlias, Alias, PublicKeyAccount}
import scorex.block.Block
import scorex.transaction.assets.IssueTransaction
import scorex.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import scorex.transaction.{CreateAliasTransaction, GenesisTransaction, PaymentTransaction, SignedTransaction}
import scorex.utils.ScorexLogging


trait StateWriter {
  def applyBlockDiff(blockDiff: BlockDiff, block: Block, newHeight: Int): Unit

  def clear(): Unit
}

class StateWriterImpl(ds: DataSource) extends StateReader with StateWriter {
  private def readOnly[A](f: DBSession => A): A = using(DB(ds.getConnection))(_.readOnly(f))

  override def nonZeroLeaseBalances = readOnly { implicit s =>
    sql"""with last_balance as (select address, max(height) height from lease_balances group by address)
         |select lb.* from lease_balances lb, last_balance
         |where lb.address = last_balance.address
         |and lb.height = last_balance.height
         |and (lease_in <> 0 or lease_out <> 0)""".stripMargin
      .map(rs => Address.fromBytes(rs.get[Array[Byte]](1)).right.get -> LeaseInfo(rs.get[Long](2), rs.get[Long](3)))
      .list()
      .apply()
      .toMap
  }

  override def accountPortfolio(a: Address) = ???

  override def transactionInfo(id: ByteStr) = ???

  override def containsTransaction(id: ByteStr) = readOnly { implicit s =>
    sql"select count(*) from transaction_offsets where tx_id = ?"
      .bind(id.arr)
      .map(_.get[Int](1))
      .single()
      .apply()
      .isEmpty
  }

  private val balanceCache = CacheBuilder
    .newBuilder()
    .maximumSize(100000).build(new CacheLoader[Address, WavesBalance] {
      override def load(key: Address) = readOnly { implicit s =>
        sql"""select top 1 wb.regular_balance, wb.effective_balance
             |from waves_balances wb
             |where wb.address = ?
             |order by wb.height desc""".stripMargin
          .bind(key.bytes.arr)
          .map { rs => WavesBalance(rs.get[Long](1), rs.get[Long](2)) }
          .single()
          .apply()
          .getOrElse(WavesBalance(0, 0))
      }
    })

  override def wavesBalance(a: Address) = balanceCache.get(a)

  override def leaseInfo(a: Address) = readOnly { implicit s =>
    sql"select top 1 lease_in, lease_out from lease_balances where address = ? order by height desc"
      .bind(a.bytes.arr)
      .map(rs => LeaseInfo(rs.get[Long](1), rs.get[Long](2)))
      .single()
      .apply()
      .getOrElse(LeaseInfo(0, 0))
  }

  private val assetBalanceCache = CacheBuilder
    .newBuilder()
    .maximumSize(100000)
    .build(new CacheLoader[Address, Map[ByteStr, Long]] {
      override def load(key: Address) = readOnly { implicit s =>
        sql"""with latest_heights as (
             |  select address, asset_id, max(height) height
             |  from asset_balances
             |  where address = ? group by address, asset_id)
             |select ab.asset_id, ab.balance from asset_balances ab, latest_heights lh
             |where ab.height = lh.height
             |and ab.asset_id = lh.asset_id
             |and ab.address = lh.address""".stripMargin
          .bind(key.bytes.arr)
          .map(rs => ByteStr(rs.get[Array[Byte]](1)) -> rs.get[Long](2))
          .list()
          .apply()
          .toMap
      }
    })

  override def assetBalance(a: Address) = assetBalanceCache.get(a)

  override def assetInfo(id: ByteStr) = readOnly { implicit s =>
    sql"select top 1 reissuable, quantity from asset_quantity where asset_id = ? order by height desc"
      .bind(id.arr)
      .map { rs => AssetInfo(rs.get[Boolean](1), rs.get[Long](2)) }
      .single()
      .apply()
  }

  override def assetDescription(id: ByteStr) = readOnly { implicit s =>
    sql"""select top 1 ai.issuer, ai.name, ai.description, ai.decimals, aq.reissuable, aq.quantity
         |from asset_info ai, asset_quantity aq
         |where ai.asset_id = aq.asset_id
         |and ai.asset_id = ?
         |order by aq.height desc""".stripMargin
      .bind(id.arr)
      .map { rs => AssetDescription(
        PublicKeyAccount(rs.get[Array[Byte]](1)),
        rs.get[Array[Byte]](2),
        rs.get[Array[Byte]](3),
        rs.get[Int](4),
        AssetInfo(rs.get[Boolean](5), rs.get[Long](6))) }
      .single()
      .apply()
  }

  override def height = readOnly { implicit s =>
    sql"select ifnull(max(height), 0) from blocks".map(_.get[Int](1)).single().apply().getOrElse(0)
  }

  override def accountTransactionIds(a: Address, limit: Int) = ???

  override def paymentTransactionIdByHash(hash: ByteStr) = using(DB(ds.getConnection)) { db =>
    db.readOnly { implicit s =>
      sql"select top 1 * from payment_transactions where tx_hash = ?"
        .bind(hash.arr)
        .map(rs => ByteStr(rs.get[Array[Byte]](1)))
        .single()
        .apply()
    }
  }

  override def aliasesOfAddress(a: Address) = readOnly { implicit s =>
    sql"select alias from aliases where address = ?"
      .bind(a.bytes.arr)
      .map(rs => Alias.fromString(rs.get[String](1)).right.get)
      .list()
      .apply()
  }

  override def resolveAlias(a: Alias) = readOnly { implicit s =>
    sql"select address from aliases where alias = ?"
      .bind(a.bytes.arr)
      .map(rs => Address.fromBytes(rs.get[Array[Byte]](1)).right.get)
      .single()
      .apply()
  }

  override def activeLeases = readOnly { implicit s =>
    sql"select lease_id from (select lease_id, bool_and(active) still_active from lease_status group by lease_id) where still_active"
      .map(rs => ByteStr(rs.get[Array[Byte]](1)))
      .list()
      .apply()
  }

  override def leaseDetails(leaseId: ByteStr) = readOnly { implicit s =>
    sql"""with this_lease_status as (
         |select lease_id, bool_and(active) active from lease_status where lease_id = ? group by lease_id)
         |select li.*, tl.active from lease_info li, this_lease_status tl
         |where li.lease_id = tl.lease_id""".stripMargin
      .bind(leaseId.arr)
        .map(rs => LeaseDetails(
          PublicKeyAccount(rs.get[Array[Byte]](2)),
          AddressOrAlias.fromBytes(rs.get[Array[Byte]](3), 0).right.get._1,
          rs.get[Int](5),
          rs.get[Long](4),
          rs.get[Boolean](6)))
      .single()
      .apply()
  }

  override def lastUpdateHeight(acc: Address) = readOnly { implicit s =>
    sql"select top 1 height from waves_balances where address = ? order by height desc"
      .bind(acc.bytes.arr)
      .map(_.get[Option[Int]](1)).single.apply().flatten
  }

  override def snapshotAtHeight(acc: Address, h: Int) = ???

  override def filledVolumeAndFee(orderId: ByteStr) = readOnly { implicit s =>
    sql"""with this_order as (select cast(? as binary) order_id)
         |select top 1 ifnull(fq.filled_quantity, 0), ifnull(fq.fee, 0) from this_order to
         |left join filled_quantity fq on to.order_id = fq.order_id
         |order by fq.height desc""".stripMargin
      .bind(orderId.arr)
      .map(rs => OrderFillInfo(rs.get[Long](1), rs.get[Long](2)))
      .single()
      .apply()
      .getOrElse(OrderFillInfo(0, 0))
  }

  override def clear(): Unit = ???

  override def applyBlockDiff(blockDiff: BlockDiff, block: Block, newHeight: Int): Unit = {
    using(DB(ds.getConnection)) { db =>
      db.localTx { implicit s =>
        for {
          (address, ls) <- blockDiff.snapshots
        } {
          ls.wavesBalance.foreach(balanceCache.put(address, _))

          if (ls.assetBalances.nonEmpty) {
            assetBalanceCache.put(address, assetBalanceCache.get(address) ++ ls.assetBalances)
          }
        }

        sql"""insert into blocks (height, block_id, block_timestamp, generator_address, block_data_bytes)
             |values (?,?,?,?,?)""".stripMargin
          .bind(newHeight, block.uniqueId.arr, new Timestamp(block.timestamp),
            block.signerData.generator.toAddress.bytes.arr, block.bytes)
          .update()
          .apply()

        sql"insert into transaction_offsets (tx_id, signature, tx_type, start_offset, height) values (?,?,?,?,?)"
          .batch(blockDiff.txsDiff.transactions.values.map {
            case (_, pt: PaymentTransaction, _) => Seq(pt.hash, pt.signature.arr, pt.transactionType.id, 0, newHeight)
            case (_, t: SignedTransaction, _) => Seq(t.id.arr, t.signature.arr, t.transactionType.id, 0, newHeight)
            case (_, gt: GenesisTransaction, _) => Seq(gt.id.arr, gt.signature.arr, gt.transactionType.id, 0, newHeight)
          }.toSeq: _*)
          .apply()

        val issuedAssetParams = blockDiff.txsDiff.transactions.values.collect {
          case (_, i: IssueTransaction, _) =>
            Seq(i.assetId.arr, i.sender.publicKey, i.decimals, i.name, i.description, newHeight): Seq[Any]
        }.toSeq

        sql"insert into asset_info(asset_id, issuer, decimals, name, description, height) values (?,?,?,?,?,?)"
          .batch(issuedAssetParams: _*)
          .apply()

        sql"""insert into asset_quantity
             |with this_asset as (select cast(? as binary) asset_id)
             |select top 1 ta.asset_id, ifnull(aq.quantity, 0) + ?, ?, ?
             |from this_asset ta
             |left join asset_quantity aq on ta.asset_id = aq.asset_id
             |order by aq.height desc""".stripMargin
          .batch(blockDiff.txsDiff.issuedAssets.map {
            case (id, ai) => Seq(id.arr, ai.volume, ai.isReissuable, newHeight)
          }.toSeq: _*)
          .apply()

        sql"""insert into filled_quantity
             |with this_order as (select cast(? as binary) order_id)
             |select this_order.order_id, ifnull(fq.filled_quantity, 0) + ?, ifnull(fq.fee, 0) + ?, ? from this_order
             |left join filled_quantity fq on this_order.order_id = fq.order_id
             |order by fq.height desc
             |limit 1""".stripMargin
          .batch((for {
            (orderId, fillInfo) <- blockDiff.txsDiff.orderFills
          } yield Seq(orderId.arr, fillInfo.volume, fillInfo.fee, newHeight)).toSeq: _*)
            .apply()

        sql"insert into lease_info (lease_id, sender, recipient, amount, height) values (?,?,?,?,?)"
          .batch(blockDiff.txsDiff.transactions.collect {
            case (_, (_, lt: LeaseTransaction, _)) =>
              Seq(lt.id.arr, lt.sender.publicKey,
                lt.recipient.bytes.arr, lt.amount, newHeight)
          }.toSeq: _*)
          .apply()

        sql"insert into lease_status (lease_id, active, height) values (?,?,?)"
          .batch(blockDiff.txsDiff.transactions.collect {
            case (_, (_, lt: LeaseTransaction, _)) => Seq(lt.id.arr, true, newHeight)
            case (_, (_, lc: LeaseCancelTransaction, _)) => Seq(lc.leaseId.arr, false, newHeight)
          }.toSeq: _*)
          .apply()

        sql"""insert into lease_balances (address, lease_in, lease_out, height)
             |with this_lease as (select cast(? as binary) address)
             |select top 1 tl.address, ifnull(lb.lease_in, 0) + ?, ifnull(lb.lease_out, 0) + ?, ?
             |from this_lease tl
             |left join lease_balances lb on tl.address = lb.address
             |order by lb.height desc""".stripMargin
          .batch((for {
            (address, p) <- blockDiff.txsDiff.portfolios
            if p.leaseInfo.leaseIn != 0 || p.leaseInfo.leaseOut != 0
          } yield Seq(address.bytes.arr, p.leaseInfo.leaseIn, p.leaseInfo.leaseOut, newHeight)).toSeq: _*)
          .apply()

        sql"""insert into waves_balances (address, regular_balance, effective_balance, height)
             |values(?, ?, ?, ?)""".stripMargin
          .batch((for {
            (address, s) <- blockDiff.snapshots
            ls <- s.wavesBalance
          } yield Seq(address.bytes.arr, ls.regularBalance, ls.effectiveBalance, newHeight)).toSeq: _*)
          .apply()

        val assetBalanceParams = for {
          (address, ls) <- blockDiff.snapshots
          (assetId, balance) <- ls.assetBalances
          _ = require(balance >= 0, s"Balance $balance <= 0 for address X'0${BigInt(address.bytes.arr).toString(16)}' asset X'${BigInt(assetId.arr).toString(16)}'")
        } yield Seq(address.bytes.arr, assetId.arr, balance, newHeight)
        sql"insert into asset_balances (address, asset_id, balance, height) values (?,?,?,?)"
          .batch(assetBalanceParams.toSeq: _*)
          .apply()

        sql"insert into aliases (alias, address, height) values (?,?,?)"
          .batch(blockDiff.txsDiff.transactions.values.collect {
            case (_, cat: CreateAliasTransaction, _) => Seq(cat.alias.bytes.arr, cat.sender.toAddress.bytes.arr, newHeight)
          }.toSeq: _*)
          .apply()

        sql"insert into address_transaction_ids (address, tx_id, signature, height) values (?,?,?,?)"
          .batch((for {
              (_, (_, tx, addresses)) <- blockDiff.txsDiff.transactions
              address <- addresses
            } yield tx match {
              case pt: PaymentTransaction => Seq(address.bytes.arr, pt.hash, pt.signature.arr, newHeight)
              case t: SignedTransaction => Seq(address.bytes.arr, t.id.arr, t.signature.arr, newHeight)
              case gt: GenesisTransaction => Seq(address.bytes.arr, gt.id.arr, gt.signature.arr, newHeight)
            }).toSeq: _*)
          .apply()
      }
    }
  }
}

object StateWriterImpl extends ScorexLogging {

  private def withTime[R](f: => R): (R, Long) = {
    val t0 = System.currentTimeMillis()
    val r: R = f
    val t1 = System.currentTimeMillis()
    (r, t1 - t0)
  }

  def measureLog[R](s: String)(f: => R): R = {
    val (r, time) = withTime(f)
    log.trace(s"$s took ${time}ms")
    r
  }
}
