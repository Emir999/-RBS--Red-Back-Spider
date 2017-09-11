package com.wavesplatform.state2.reader

import scorex.account.PublicKeyAccount

case class LeaseInfo(sender: PublicKeyAccount, recipient: PublicKeyAccount, height: Int, amount: Long, isActive: Boolean)
