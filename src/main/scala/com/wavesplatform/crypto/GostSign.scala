package com.wavesplatform.crypto

import java.security._

import ru.CryptoPro.JCP.JCP
import scorex.crypto.encode.Base58
import scorex.crypto.signatures.SigningFunctions
import scorex.crypto.signatures.SigningFunctions.{MessageToSign, Signature}
import sun.security.jca.JCAUtil
import userSamples.KeyPairGen

object GostSign {
  def sign(privateKey: PrivateKey, message: MessageToSign): SigningFunctions.Signature = {
    val sig = Signature.getInstance("GOST3411withGOST3410EL")
    sig.initSign(privateKey)
    sig.update(message)
    sig.sign()
  }

  def verify(signature: Signature, message: MessageToSign, publicKey: PublicKey): Boolean = {
    val sig = Signature.getInstance("GOST3411withGOST3410EL")
    sig.initVerify(publicKey)
    sig.verify(message)
  }

  def createKeyPair(seed: Array[Byte]): (PrivateKey, PublicKey) = {
    val kg = KeyPairGenerator.getInstance(
      JCP.GOST_EL_2012_256_NAME, JCP.PROVIDER_NAME)
    kg.initialize(32, JCAUtil.getSecureRandom)
    val pair = kg.generateKeyPair
    (pair.getPrivate, pair.getPublic)
  }
}
object Test extends App {
  val kg = KeyPairGenerator.getInstance(JCP.GOST_EL_2012_256_NAME, JCP.PROVIDER_NAME)
  kg.initialize(32, JCAUtil.getSecureRandom)
  val pair = kg.generateKeyPair
  val keyStore = KeyStore.getInstance(JCP.HD_STORE_NAME, JCP.PROVIDER_NAME)
  keyStore.load(null, null)
  val alias = "123"
  val password = "111".toCharArray
  println(Base58.encode(pair.getPublic.getEncoded))
  keyStore.setKeyEntry(alias, pair.getPrivate, password, Array(KeyPairGen.genSelfCert(pair, "CN=Waves_2012_512, O=Waves, C=RU")))
//  keyStore.store()
  println(Base58.encode(keyStore.getCertificate(alias).getPublicKey.getEncoded))
}
