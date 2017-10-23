package com.wavesplatform.state2

import scorex.block.Block
import scorex.transaction.Transaction


trait StateWriter {
  def append(blockDiff: BlockDiff, block: Block): Unit
  def rollbackTo(targetBlockId: ByteStr): Seq[Transaction]
}
