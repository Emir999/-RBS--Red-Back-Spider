package scorex.account

import java.security.PublicKey

import com.objsys.asn1j.runtime.Asn1BerEncodeBuffer
import ru.CryptoPro.JCP.Key.GostPublicKey
import scorex.crypto.encode.Base58
import scorex.transaction.ValidationError.InvalidAddress
import scorex.transaction.{TransactionParser, ValidationError}

import scala.language.implicitConversions


trait PublicKeyAccount {
  def publicKey: PublicKey

  override def equals(b: Any): Boolean = b match {
    case a: PublicKeyAccount => publicKey.equals(a.publicKey)
    case _ => false
  }

  override def hashCode(): Int = publicKey.hashCode()

  override lazy val toString: String = this.toAddress.address
}

object PublicKeyAccount {

  private case class PublicKeyAccountImpl(publicKey: PublicKey) extends PublicKeyAccount

  def apply(publicKey: Array[Byte]): PublicKeyAccount = {PublicKeyAccountImpl(new GostPublicKey(publicKey))}

  implicit def toAddress(publicKeyAccount: PublicKeyAccount): Address = Address.fromPublicKey(publicKeyAccount.publicKey.getEncoded)

  implicit class PublicKeyAccountExt(pk: PublicKeyAccount) {
    def toAddress: Address = PublicKeyAccount.toAddress(pk)
  }

  def fromBase58String(s: String): Either[ValidationError, PublicKeyAccount] =
    if (s.length > TransactionParser.KeyStringLength) Left(InvalidAddress)
    else Base58.decode(s).toOption.map(PublicKeyAccount(_)).toRight(InvalidAddress)
}
