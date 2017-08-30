package scorex.account

import java.security.{PrivateKey, PublicKey}

sealed trait PrivateKeyAccount extends PublicKeyAccount {
  def privateKey: PrivateKey
}

object PrivateKeyAccount {

  private case class PrivateKeyAccountImpl(privateKey: PrivateKey, publicKey: PublicKey) extends PrivateKeyAccount

  def apply(privateKey: PrivateKey, publicKey: PublicKey): PrivateKeyAccount = {
    PrivateKeyAccountImpl(privateKey, publicKey)
  }
}
