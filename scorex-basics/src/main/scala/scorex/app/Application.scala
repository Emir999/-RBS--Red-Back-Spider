package scorex.app

import akka.actor.{Props, ActorSystem}
import akka.io.IO
import scorex.api.http.{ApiRoute, CompositeHttpServiceActor}
import scorex.block.Block
import scorex.consensus.ConsensusModule
import scorex.network.message.{BasicMessagesRepo, MessageHandler}
import scorex.network.{BlockchainSyncer, NetworkController}
import scorex.settings.Settings
import scorex.transaction.{History, State, TransactionModule}
import scorex.utils.ScorexLogging
import scorex.wallet.Wallet
import spray.can.Http
import scala.reflect.runtime.universe.Type

import scala.concurrent.ExecutionContext.Implicits.global


trait Application extends ScorexLogging {
  val applicationName:String

  //settings
  implicit val settings: Settings

  //modules
  implicit val consensusModule : ConsensusModule[_]
  implicit val transactionModule: TransactionModule[_]

  //api
  val apiRoutes: Seq[ApiRoute]
  val apiTypes: Seq[Type]

  protected implicit lazy val actorSystem = ActorSystem("lagonaki")
  lazy val apiActor = actorSystem.actorOf(Props(classOf[CompositeHttpServiceActor], apiTypes, apiRoutes), "api")


  //p2p
  val messagesHandler:MessageHandler

  lazy val networkController = actorSystem.actorOf(Props(classOf[NetworkController], this))
  lazy val blockchainSyncer = actorSystem.actorOf(Props(classOf[BlockchainSyncer], this, networkController))

  //wallet
  private lazy val walletFileOpt = settings.walletDirOpt.map(walletDir => new java.io.File(walletDir, "wallet.s.dat"))
  implicit lazy val wallet = new Wallet(walletFileOpt, settings.walletPassword, settings.walletSeed.get)

  //interface to append log and state
  val history: History
  val state: State


  def run() {
    checkGenesis()

    blockchainSyncer ! Unit //initializing

    IO(Http) ! Http.Bind(apiActor, interface = "0.0.0.0", port = settings.rpcPort)

    //CLOSE ON UNEXPECTED SHUTDOWN
    Runtime.getRuntime.addShutdownHook(new Thread() {
      override def run() {
        stopAll()
      }
    })
  }

  def stopAll() = synchronized {
    log.info("Stopping message processor")
    networkController ! NetworkController.ShutdownNetwork

    log.info("Stopping actors (incl. block generator)")
    actorSystem.terminate().onComplete { _ =>
      //CLOSE WALLET
      log.info("Closing wallet")
      wallet.close()

      //FORCE CLOSE
      log.info("Exiting from the app...")
      System.exit(0)
    }
  }

  def checkGenesis(): Unit = {
    if (transactionModule.history.isEmpty) {
      val genesisBlock = Block.genesis()
      transactionModule.state.processBlock(genesisBlock)
      transactionModule.history.appendBlock(genesisBlock).ensuring(_.height() == 1)
      log.info("Genesis block has been added to the state")
    }
  }.ensuring(transactionModule.history.height() >= 1)
}