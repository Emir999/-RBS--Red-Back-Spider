package com.wavesplatform.state2

import javax.sql.DataSource

import com.wavesplatform.state2.reader.StateReader
import scalikejdbc._
import scorex.account.{Address, Alias}
import scorex.transaction.assets.IssueTransaction
import scorex.transaction.lease.LeaseTransaction


trait StateWriter {
  def applyBlockDiff(blockDiff: BlockDiff, blockBytes: Array[Byte], newHeight: Int): Unit

  def clear(): Unit
}

class StateWriterImpl(ds: DataSource) extends StateReader with StateWriter {
  implicit def toParamBinder[A](v: A)(implicit ev: ParameterBinderFactory[A]): ParameterBinder = ev(v)

  private def readOnly[A](f: DBSession => A): A = using(DB(ds.getConnection))(_.readOnly(f))

  override def accountPortfolios = readOnly { implicit session =>
    Map.empty[Address, Portfolio]
  }

  override def accountPortfolio(a: Address) = ???

  override def transactionInfo(id: ByteStr) = ???

  override def containsTransaction(id: ByteStr) = using(DB(ds.getConnection)) { db =>
    db.readOnly { implicit s =>
      sql"select count(*) from transaction_offsets where tx_id = ?"
        .bind(id.arr: ParameterBinder)
        .map(_.get[Int](1))
        .single()
        .apply()
        .isEmpty
    }
  }

  override def wavesBalance(a: Address) = readOnly { implicit s =>
    sql"""select top 1 wb.balance, wb.lease_in, wb.lease_out
          from waves_balances wb
          where wb.address = ?
          order by height desc"""
      .bind(a.bytes.arr: ParameterBinder)
      .map { rs => WavesBalance(rs.get[Long](1), LeaseInfo(rs.get[Long](2), rs.get[Long](3))) }
      .single()
      .apply()
      .getOrElse(WavesBalance(0, LeaseInfo.empty))
  }

  override def assetBalance(a: Address, assetId: ByteStr) = readOnly { implicit s =>
    sql"select top 1 ab.balance from asset_balances ab where ab.address = ? and ab.asset_id = ? order by ab.height desc"
      .bind(a.bytes.arr: ParameterBinder, assetId.arr: ParameterBinder)
      .map(_.get[Long](1))
      .single()
      .apply()
      .getOrElse(0L)
  }

  override def assetInfo(id: ByteStr) = ???

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

  override def aliasesOfAddress(a: Address) = ???

  override def resolveAlias(a: Alias) = ???

  override def isLeaseActive(leaseTx: LeaseTransaction) = ???

  override def activeLeases() = ???

  override def lastUpdateHeight(acc: Address) = using(DB(ds.getConnection)) { db =>
    db.readOnly { implicit s =>
      sql"select max(height) from waves_balances where address = ?"
        .bind(acc.bytes.arr: ParameterBinder)
        .map(_.get[Option[Int]](1)).single.apply().flatten
    }
  }

  override def snapshotAtHeight(acc: Address, h: Int) = ???

  override def filledVolumeAndFee(orderId: ByteStr) = ???

  override def clear(): Unit = ???

  override def applyBlockDiff(blockDiff: BlockDiff, blockBytes: Array[Byte], newHeight: Int): Unit = {
    using(DB(ds.getConnection)) { db =>
      db.localTx { implicit s =>

        sql"insert into blocks (height, block_id, block_data_bytes) values (?, cast(0 as binary), cast(? as binary))"
          .bind(newHeight, blockBytes: ParameterBinder)
          .update()
          .apply()

//        sql"insert into transaction_offsets (tx_id, height, start_offset) values (?, ?, ?)"
//          .batch(blockDiff.txsDiff.transactions.keys.map(k => Seq(k.arr: ParameterBinder, newHeight, 0)).toSeq: _*)
//          .apply()

        val issuedAssetParams = blockDiff.txsDiff.transactions.values.collect {
          case (_, i: IssueTransaction, _) =>
            Seq(i.assetId.arr: ParameterBinder, i.decimals, i.name: ParameterBinder, i.description: ParameterBinder, newHeight)
        }

        sql"insert into asset_info(asset_id, decimals, name, description, height) values (?,?,?,?,?)"
          .batch(issuedAssetParams.toSeq: _*)
          .apply()

        sql"insert into asset_quantity(asset_id, quantity, reissuable, height) values (?,?,?,?)"
          .batch(blockDiff.txsDiff.issuedAssets.map {
            case (id, ai) => Seq(id.arr: ParameterBinder, ai.volume, ai.isReissuable, newHeight)
          }.toSeq: _*)
          .apply()

        sql"""insert into filled_quantity
             |with this_order as (select cast(? as binary) order_id)
             |select this_order.order_id, ifnull(fq.filled_quantity, 0) + ?, ? from this_order
             |left join filled_quantity fq on this_order.order_id = fq.order_id
             |order by fq.height desc
             |limit 1""".stripMargin
          .batch((for {
            (orderId, fillInfo) <- blockDiff.txsDiff.orderFills
          } yield Seq(orderId.arr: ParameterBinder, fillInfo.volume, newHeight)).toSeq: _*)
            .apply()

        sql"""insert into asset_balances
             |with this_balance as (select cast(? as binary) address, cast(? as binary) asset_id)
             |select this_balance.address, this_balance.asset_id, ifnull(ab.balance, 0) + ?, ? from this_balance
             |left join asset_balances ab on this_balance.address = ab.address and this_balance.asset_id = ab.asset_id
             |order by ab.height desc
             |limit 1""".stripMargin
          .batch((for {
            (address, p) <- blockDiff.txsDiff.portfolios
            (assetId, balance) <- p.assets
          } yield Seq(address.bytes.arr: ParameterBinder, assetId.arr: ParameterBinder, balance, newHeight)).toSeq: _*)
          .apply()

        sql"""insert into waves_balances (address, balance, lease_in, lease_out, height)
             |values(?, ?, ?, ?, ?)""".stripMargin
          .batch((for {
            (address, s) <- blockDiff.snapshots
            (_, ls) = s.last
          } yield Seq(address.bytes.arr: ParameterBinder, ls.balance, 0, 0, newHeight)).toSeq: _*)
          .apply()
      }
    }
  }
}
