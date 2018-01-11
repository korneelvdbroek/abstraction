package com.fbot.common.linalg.distributed

import org.apache.spark.mllib.linalg.Matrix
import org.apache.spark.mllib.linalg.distributed.BlockMatrix
import org.apache.spark.rdd.RDD
import RichBlockMatrix._

import scala.annotation.tailrec
import scala.reflect.ClassTag

/**
  * Copyright (C) 2017-2018  korneelvdbroek
  *
  * This program is free software: you can redistribute it and/or modify
  * it under the terms of the GNU General Public License as published by
  * the Free Software Foundation, either version 3 of the License, or
  * (at your option) any later version.
  *
  * This program is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU General Public License for more details.
  *
  * You should have received a copy of the GNU General Public License
  * along with this program.  If not, see <http://www.gnu.org/licenses/>.
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
