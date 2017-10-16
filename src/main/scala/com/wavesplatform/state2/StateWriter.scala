package com.wavesplatform.state2

import scorex.block.Block
import scorex.transaction.Transaction


trait StateWriter {
  def applyBlockDiff(blockDiff: BlockDiff, block: Block, newHeight: Int): Unit
  def rollbackToHeight(targetHeight: Int): Seq[Transaction]
  def clear(): Unit
}

