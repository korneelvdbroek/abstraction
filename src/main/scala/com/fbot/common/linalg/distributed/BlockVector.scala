package com.fbot.common.linalg.distributed

import org.apache.spark.mllib.linalg.Matrix
import org.apache.spark.mllib.linalg.distributed.BlockMatrix
import org.apache.spark.rdd.RDD

/**
  *
  */
class BlockVector(override val blocks: RDD[((Int, Int), Matrix)],
                  override val rowsPerBlock: Int) extends BlockMatrix(blocks, rowsPerBlock, 1, 0L, 0L)

object BlockVector {

  type VectorBlock = (Int, Matrix)

  def apply(blocks: RDD[VectorBlock], rowsPerBlock: Int): BlockVector = {
    new BlockVector(blocks.map(x => ((x._1, 0), x._2)), rowsPerBlock)
  }

}
