package com.wavesplatform.it.transactions

import com.wavesplatform.it.api.NodeApi.Transaction
import com.wavesplatform.it.util._
import com.wavesplatform.it.{IntegrationSuiteWithThreeAddresses, Node}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future.traverse
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.Random


class MicroblocksFeeSpecification(override val allNodes: Seq[Node], override val notMiner: Node)
  extends IntegrationSuiteWithThreeAddresses {

//  private def txRequestsGen(n: Int, fee: Long): Future[Seq[Transaction]] = Future.sequence {
//    (1 to n).map { _ =>
//      sender.transfer(sender.address, firstAddress, (1 + Random.nextInt(10)).waves, fee)
//    }
//  }

  private def txRequestsGen(n: Int, fee: Long): Future[Unit] = {
    val parallelRequests = 1

    def requests(n: Int): Future[Unit] = Future
      .sequence {
        (1 to n).map { _ => sender.transfer(sender.address, firstAddress, (1 + Random.nextInt(10)).waves, fee) }
      }
      .map(_ => ())

    val steps = (1 to n)
      .sliding(parallelRequests, parallelRequests)
      .map(_.size)

    steps.foldLeft(Future.successful(())) { (r, numRequests) => r.flatMap(_ => requests(numRequests)) }
  }

  private def minerAndFee(blockHeight: Int): Future[(Node, Long)] = for {
    blockInfo <- sender.blockAt(blockHeight)
  } yield {
    val miningNode = allNodes.find(_.address == blockInfo.generator).get
    (miningNode, blockInfo.fee)
  }

  test("microblockheightactivation  and fee") {
    val microblockActivationHeight = 21

    val f = for {
      height <- traverse(allNodes)(_.height).map(_.max)

      _ <- traverse(allNodes)(_.waitForHeight(microblockActivationHeight - 2))
      _ <- txRequestsGen(200, 2.waves)
      _ <- traverse(allNodes)(_.waitForHeight(microblockActivationHeight + 1))

      initialBalances <- sender.debugStateAt(microblockActivationHeight - 2) //100%

      balancesBeforeActivation <- sender.debugStateAt(microblockActivationHeight - 1) // 100%
      blockBeforeActivation <- sender.blockAt(microblockActivationHeight - 1)

      balancesOnActivation <- sender.debugStateAt(microblockActivationHeight) // 40%
      blockOnActivation <- sender.blockAt(microblockActivationHeight)

      balancesAfterActivation <- sender.debugStateAt(microblockActivationHeight + 1) // 60% of previous + 40% of current
      blockAfterActivation <- sender.blockAt(microblockActivationHeight + 1)
    } yield {

      balancesBeforeActivation(blockBeforeActivation.generator) shouldBe {
        initialBalances(blockBeforeActivation.generator) + blockBeforeActivation.fee
      }

      balancesOnActivation(blockOnActivation.generator) shouldBe {
        balancesBeforeActivation(blockOnActivation.generator) + blockOnActivation.fee * 0.4
      }

      balancesAfterActivation(blockAfterActivation.generator) shouldBe {
        balancesOnActivation(blockAfterActivation.generator) +
          blockOnActivation.fee * 0.6 + blockAfterActivation.fee * 0.4
      }
    }

    Await.result(f, 2.minute)
  }
}
