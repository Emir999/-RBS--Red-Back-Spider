package scorex.wallet

import java.io.{File, FileInputStream}
import java.security.{KeyPairGenerator, KeyStore, PrivateKey}

import com.wavesplatform.crypto.Test.{alias, keyStore, pair, password}
import com.wavesplatform.settings.WalletSettings
import ru.CryptoPro.JCP.JCP
import scorex.account.{Address, PrivateKeyAccount, PublicKeyAccount}
import scorex.transaction.ValidationError
import scorex.transaction.ValidationError.MissingSenderPrivateKey
import scorex.utils.ScorexLogging
import sun.security.jca.JCAUtil
import userSamples.KeyPairGen

import scala.util.{Failure, Success, Try}

class Wallet private(file: Option[File], password: Array[Char]) extends AutoCloseable with ScorexLogging {

  private val NonceFieldName = "nonce"

  private val kg = KeyPairGenerator.getInstance(JCP.GOST_EL_2012_256_NAME, JCP.PROVIDER_NAME)
  kg.initialize(32, JCAUtil.getSecureRandom)

  val keyStore = KeyStore.getInstance(JCP.HD_STORE_NAME, JCP.PROVIDER_NAME)
  file.foreach(f => keyStore.load(new FileInputStream(f), password))

  def generateNewAccounts(howMany: Int): Seq[PrivateKey] =
    (1 to howMany).flatMap(_ => generateNewAccount())

  def generateNewAccount(): Option[PrivateKey] = synchronized {
    val pair = kg.generateKeyPair
    val address = PublicKeyAccount(pair.getPublic.getEncoded).address
    keyStore.setKeyEntry(address, pair.getPrivate, password, Array(KeyPairGen.genSelfCert(pair, "CN=Waves_2012_512, O=Waves, C=RU")))
    Some(pair.getPrivate)
  }

  def deleteAccount(account: PrivateKeyAccount): Boolean = synchronized {
    keyStore.deleteEntry(account.address)
  }

  def privateKeyAccount(account: Address): Either[ValidationError, PrivateKeyAccount] = {
    Try(keyStore.getKey(account.address, password)) match {
      // todo construct it
      case Success(k) => Right(PrivateKeyAccount(k))
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

  def generateNewAccount(): PrivateKeyAccount = PrivateKeyAccount()

  def apply(settings: WalletSettings): Wallet = {
    new Wallet(settings.file, settings.password.toCharArray)
  }
}
