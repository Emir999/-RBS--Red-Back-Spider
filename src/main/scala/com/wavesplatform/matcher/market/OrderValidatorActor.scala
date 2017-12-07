package com.wavesplatform.matcher.market

import akka.actor.{Actor, Props}
import com.wavesplatform.UtxPool
import com.wavesplatform.matcher.MatcherSettings
import com.wavesplatform.matcher.market.OrderBookActor.CancelOrder
import com.wavesplatform.matcher.market.OrderHistoryActor._
import com.wavesplatform.matcher.market.OrderValidatorActor.{ValidateCancelOrder, ValidateCancelResult, ValidateOrder, ValidateOrderResult}
import com.wavesplatform.matcher.model._
import scorex.transaction.ValidationError.GenericError
import scorex.transaction.assets.exchange.Order
import scorex.utils.NTP
import scorex.wallet.Wallet

class OrderValidatorActor(val settings: MatcherSettings, val utxPool: UtxPool, val wallet: Wallet, val orderHistory: OrderHistory)
  extends Actor with OrderValidator {

  def processExpirableRequest(r: Any): Unit = r match {
    case ValidateOrder(o, ts) =>
      sender() ! ValidateOrderResult(validateNewOrder(o))
    case ValidateCancelOrder(co, _) =>
      sender() ! ValidateCancelResult(validateCancelOrder(co))
  }

  override def receive: Receive = {
    case req: ExpirableOrderHistoryRequest =>
      if (NTP.correctedTime() - req.ts < RequestTTL) {
        processExpirableRequest(req)
      }
  }

}

object OrderValidatorActor {

  def name = "OrderValidator"
  def props(settings: MatcherSettings, utxPool: UtxPool, wallet: Wallet, orderHistory: OrderHistory): Props =
    Props(new OrderValidatorActor(settings, utxPool, wallet, orderHistory))

  case class ValidateOrder(order: Order, ts: Long) extends ExpirableOrderHistoryRequest
  case class ValidateOrderResult(result: Either[GenericError, Order])
  case class ValidateCancelOrder(cancel: CancelOrder, ts: Long) extends ExpirableOrderHistoryRequest
  case class ValidateCancelResult(result: Either[GenericError, CancelOrder])
}