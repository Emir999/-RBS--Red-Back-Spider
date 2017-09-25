package scorex.wallet

import java.io.{File, FileInputStream}
import java.security.{KeyPairGenerator, KeyStore, PrivateKey}

import com.wavesplatform.settings.WalletSettings
import ru.CryptoPro.JCP.JCP
import scorex.account.{Address, PrivateKeyAccount, PublicKeyAccount}
import scorex.transaction.ValidationError
import scorex.utils.ScorexLogging
import sun.security.jca.JCAUtil
import userSamples.KeyPairGen

import scala.collection.JavaConverters._

import scala.util.{Failure, Success, Try}

class Wallet private(file: Option[File], password: Array[Char]) extends AutoCloseable with ScorexLogging {

  import Wallet._

  private val NonceFieldName = "nonce"

  val keyStore = KeyStore.getInstance(JCP.HD_STORE_NAME, JCP.PROVIDER_NAME)
  file.foreach(f => keyStore.load(new FileInputStream(f), password))

  def generateNewAccounts(howMany: Int): Seq[PublicKeyAccount] =
    (1 to howMany).flatMap(_ => generateNewAccount())

  def generateNewAccount(): Option[PublicKeyAccount] = synchronized {
    val pair = kg.generateKeyPair
    val pka = PublicKeyAccount(pair.getPublic.getEncoded)
    keyStore.setKeyEntry(pka.address, pair.getPrivate, password, Array(KeyPairGen.genSelfCert(pair, "CN=Waves_2012_512, O=Waves, C=RU")))
    Some(pka)
  }

  def deleteAccount(account: PrivateKeyAccount): Boolean = synchronized {
    Try(keyStore.deleteEntry(account.address)).isSuccess
  }

  def privateKeyAccounts(): Seq[PrivateKeyAccount] = {
    for {
      alias <- keyStore.aliases().asScala.toSeq
    } yield {
      val key = keyStore.getKey(alias, password)
      val cert = keyStore.getCertificate(alias)
      PrivateKeyAccount(key.asInstanceOf[PrivateKey], cert.getPublicKey)
    }
  }

  def privateKeyAccount(account: Address): Either[ValidationError, PrivateKeyAccount] = {
    Try(keyStore.getCertificate(account.address) -> keyStore.getKey(account.address, password)) match {
      // todo construct it
      case Success((c, k)) => Right(PrivateKeyAccount(k.asInstanceOf[PrivateKey], c.getPublicKey))
      case Failure(f) => Left(ValidationError.MissingSenderPrivateKey)
    }
  }

  def nonEmpty = keyStore.aliases().hasMoreElements

  def close(): Unit = ()
}


object Wallet extends ScorexLogging {

  implicit class WalletExtension(w: Wallet) {
    def findWallet(addressString: String): Either[ValidationError, PrivateKeyAccount] = for {
      acc <- Address.fromString(addressString)
      privKeyAcc <- w.privateKeyAccount(acc)
    } yield privKeyAcc

  }

  private val kg = KeyPairGenerator.getInstance(JCP.GOST_EL_2012_256_NAME, JCP.PROVIDER_NAME)
  kg.initialize(32, JCAUtil.getSecureRandom)

  def generateNewAccount(): PrivateKeyAccount = {
    val pair = kg.generateKeyPair
    PrivateKeyAccount(pair.getPrivate, pair.getPublic)
  }

  def apply(settings: WalletSettings): Wallet = {
    new Wallet(settings.file, settings.password.toCharArray)
  }
}
