package com.wavesplatform.state2

import scorex.block.Block


trait StateWriter {
  def applyBlockDiff(blockDiff: BlockDiff, block: Block, newHeight: Int): Unit

  def clear(): Unit
}

