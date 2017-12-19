package com.wavesplatform.it

import com.wavesplatform.it.api._
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.it.util._
import org.asynchttpclient.util.HttpConstants
import play.api.libs.json.{JsArray, JsObject, JsValue, Json}
import scorex.crypto.encode.Base58

import scala.concurrent.Await
import scala.concurrent.duration._

class TransactionsApiSuite extends BaseTransactionSuite {

  private val timeout = 2.minutes

  test("height should always be reported for transactions") {
    val f = for {
      txId <- sender.transfer(firstAddress, secondAddress, 1.waves, fee = 1.waves).map(_.id)
      _ <- waitForHeightAraiseAndTxPresent(txId, 1)

      jsv1 <- sender.get(s"/transactions/info/$txId").as[JsValue]
      hasPositiveHeight1 = (jsv1 \ "height").asOpt[Int].map(_ > 0)
      _ = assert(hasPositiveHeight1.getOrElse(false))

      jsv2 <- sender.get(s"/transactions/address/$firstAddress/limit/1").as[JsArray]
      hasPositiveHeight2 = (jsv2(0)(0) \ "height").asOpt[Int].map(_ > 0)
      _ = assert(hasPositiveHeight2.getOrElse(false))
    } yield succeed

    Await.result(f, timeout)
  }

  test("/transactions/sign should handle erroneous input") {
    def assertSignBadJson(json: JsObject) =
      assertBadRequestAndMessage(sender.postJsonWithApiKey("/transactions/sign", json), "failed to parse json message")
    val json = Json.obj(
      "type" -> 10,
      "sender" -> firstAddress,
      "alias" -> "alias",
      "fee" -> 100000)
    val f = for {
      _ <- assertSignBadJson(json - "type")
      _ <- assertSignBadJson(json + ("type" -> Json.toJson(-100)))
      _ <- assertSignBadJson(json - "alias")
    } yield succeed

    Await.result(f, timeout)
  }

  test("/transactions/sign should respect timestamp if specified") {
    val timestamp = 1500000000000L
    val json = Json.obj(
      "type" -> 10,
      "sender" -> firstAddress,
      "alias" -> "alias",
      "fee" -> 100000,
      "timestamp" -> timestamp)
    val f = for {
      r <- sender.postJsonWithApiKey("/transactions/sign", json)
      _ = assert(r.getStatusCode == HttpConstants.ResponseStatusCodes.OK_200)
      _ = assert((Json.parse(r.getResponseBody) \ "timestamp").as[Long] == timestamp)
    } yield succeed

    Await.result(f, timeout)
  }

  test("/transactions/broadcast should handle erroneous input") {
    def assertBroadcastBadJson(json: JsObject, expectedMessage: String) =
      assertBadRequestAndMessage(sender.postJson("/transactions/broadcast", json), expectedMessage)
    val timestamp = System.currentTimeMillis
    val json = Json.obj(
      "type" -> 10,
      "senderPublicKey" -> "8LbAU5BSrGkpk5wbjLMNjrbc9VzN9KBBYv9X8wGpmAJT",
      "alias" -> "alias",
      "fee" -> 100000,
      "timestamp" -> timestamp,
      "signature" -> "A" * 64)
    val f = for {
      _ <- assertBroadcastBadJson(json - "type", "failed to parse json message")
      _ <- assertBroadcastBadJson(json - "type" + ("type" -> Json.toJson(88)), "failed to parse json message")
      _ <- assertBroadcastBadJson(json - "alias", "failed to parse json message")
      _ <- assertBroadcastBadJson(json, "invalid signature")
    } yield succeed

    Await.result(f, timeout)
  }

  test("/transactions/sign should produce issue/reissue transactions that are good for /transactions/broadcast") {
    val issueId = signAndBroadcast(Json.obj(
      "type" -> 3,
      "name" -> "Gigacoin",
      "quantity" -> 100.waves,
      "description" -> "Gigacoin",
      "sender" -> firstAddress,
      "decimals" -> 8,
      "reissuable" -> true,
      "fee" -> 1.waves))

    signAndBroadcast(Json.obj(
      "type" -> 5,
      "quantity" -> 200.waves,
      "assetId" -> issueId,
      "sender" -> firstAddress,
      "reissuable" -> false,
      "fee" -> 1.waves))
  }

  test("/transactions/sign should produce transfer transaction that is good for /transactions/broadcast") {
    signAndBroadcast(Json.obj(
      "type" -> 4,
      "sender" -> firstAddress,
      "recipient" -> secondAddress,
      "fee" -> 100000,
      "amount" -> 1.waves,
      "attachment" -> Base58.encode("falafel".getBytes)))
  }

  test("/transactions/sign should produce leasing transactions that are good for /transactions/broadcast") {
    signAndBroadcast(Json.obj(
      "type" -> 8,
      "sender" -> firstAddress,
      "amount" -> 1.waves,
      "recipient" -> secondAddress,
      "fee" -> 100000))
  }

  test("/transactions/sign should produce alias transaction that is good for /transactions/broadcast") {
    signAndBroadcast(Json.obj(
      "type" -> 10,
      "sender" -> firstAddress,
      "alias" -> "myalias",
      "fee" -> 100000))
  }

  private def signAndBroadcast(json: JsObject): String = {
    val f = for {
      rs <- sender.postJsonWithApiKey("/transactions/sign", json)
      _ = assert(rs.getStatusCode == HttpConstants.ResponseStatusCodes.OK_200)
      body = Json.parse(rs.getResponseBody)
      _ = assert((body \ "signature").as[String].nonEmpty)
      rb <- sender.postJson("/transactions/broadcast", body)
      _ = assert(rb.getStatusCode == HttpConstants.ResponseStatusCodes.OK_200)
      id = (Json.parse(rb.getResponseBody) \ "id").as[String]
      _ = assert(id.nonEmpty)
      _ <- waitForHeightAraiseAndTxPresent(id, 1)
    } yield id

    Await.result(f, timeout)
  }
}
