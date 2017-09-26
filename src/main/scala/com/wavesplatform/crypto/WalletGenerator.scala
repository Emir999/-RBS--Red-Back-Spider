package com.wavesplatform.crypto

import java.io.File

import scorex.account.AddressScheme
import scorex.wallet.Wallet

object WalletGenerator extends App {
  AddressScheme.current = new AddressScheme {
    override val chainId: Byte = 'G'
  }
  val f = new File("/tmp/wallet.dat")
  if (f.exists()) f.delete()
  val w = new Wallet(Some(f), "qwertyuio".toCharArray)
  w.generateNewAccounts(5).foreach(p => println(p.address))
}
