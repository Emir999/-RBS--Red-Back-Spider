package com.wavesplatform.crypto

import java.security._

import ru.CryptoPro.JCP.JCP
import ru.CryptoPro.JCP.Key.GostPublicKey
import scorex.account.PrivateKeyAccount
import scorex.crypto.signatures.SigningFunctions
import scorex.crypto.signatures.SigningFunctions.{MessageToSign, Signature}

object GostSign {
  def sign(privateKey: PrivateKey, message: MessageToSign): SigningFunctions.Signature = {
    val sig = Signature.getInstance(JCP.GOST_SIGN_2012_256_NAME)
    sig.initSign(privateKey)
    sig.update(message)
    sig.sign()
  }

  def sign(pka: PrivateKeyAccount, message: MessageToSign): SigningFunctions.Signature = {
    sign(pka.privateKey, message)
  }

  def verify(signature: Signature, message: MessageToSign, publicKey: PublicKey): Boolean = {
    val sig = Signature.getInstance(JCP.GOST_SIGN_2012_256_NAME)
    sig.initVerify(publicKey)
    sig.update(message)
    sig.verify(signature)
  }

  def verify(signature: Signature, message: MessageToSign, publicKeyBytes: Array[Byte]): Boolean = {
    verify(signature, message, new GostPublicKey(publicKeyBytes))
  }
}