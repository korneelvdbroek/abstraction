package com.fbot.common.linalg.distributed

import org.apache.spark.mllib.linalg.Matrix
import org.apache.spark.mllib.linalg.distributed.BlockMatrix
import org.apache.spark.rdd.RDD
import RichBlockMatrix._

import scala.annotation.tailrec
import scala.reflect.ClassTag

/**
  *
  */
class BlockVector(override val blocks: RDD[((Int, Int), Matrix)],
                  val elementsPerBlock: Int) extends BlockMatrix(blocks, elementsPerBlock, 1) {

  def length: Long = numRows

  def isEmpty: Boolean = { numRows == 0 }

  def apply(i: Long): Double = this.apply(i, 0L)

  def last: Double = this(length - 1)


  def aggregate[Acc](z: Acc)(seqOp: (Acc, Double) => Acc, combOb: (Acc, Acc) => Acc)(implicit arg0: ClassTag[Acc]): Acc = {
    blocks
      .treeAggregate(z)((acc, matrixBlock) => combOb(acc, matrixBlock._2.toArray.foldLeft(z)(seqOp)), combOb)
  }

  def forallWithIndex(p: (Double, Long) => Boolean): Boolean = this.forallWithIndex((v: Double, i: Long, _: Long) => p(v, i))

  def toLocalVector: Matrix = toLocalMatrix()

  override def toString: String = toLocalMatrix().toString()

}

object BlockVector {

  type VectorBlock = (Int, Matrix)

  def apply(blocks: RDD[VectorBlock], rowsPerBlock: Int): BlockVector = {
    new BlockVector(blocks.map(x => ((x._1, 0), x._2)), rowsPerBlock)
  }


}
