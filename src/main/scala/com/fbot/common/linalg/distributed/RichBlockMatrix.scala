package com.fbot.common.linalg.distributed

import com.fbot.common.linalg.RichDenseMatrix._
import com.fbot.common.linalg.distributed.BlockVector.VectorBlock
import com.fbot.common.linalg.distributed.RichBlockMatrix.MatrixBlock
import org.apache.spark.mllib.linalg.distributed.BlockMatrix
import org.apache.spark.mllib.linalg.{DenseMatrix, Matrix}
import org.apache.spark.rdd.RDD

/**
  *
  */
class RichBlockMatrix(val matrix: BlockMatrix) extends AnyVal {

  private def blockRowIndex(i: Long): Int = (i / rowsPerBlock).toInt
  private def blockColIndex(j: Long): Int = (j / colsPerBlock).toInt
  private def subMatrixRowIndex(i: Long): Int = (i - blockRowIndex(i).toLong * rowsPerBlock).toInt
  private def subMatrixColIndex(j: Long): Int = (j - blockColIndex(j).toLong * colsPerBlock).toInt
  private def row(blockRowIndex: Int, subMatrixRowIndex: Int): Long = blockRowIndex.toLong * rowsPerBlock + subMatrixRowIndex.toLong
  private def col(blockColIndex: Int, subMatrixColIndex: Int): Long = blockColIndex.toLong * colsPerBlock + subMatrixColIndex.toLong

  def apply(i: Long, j: Long): Double = {
    blocks
      .lookup((blockRowIndex(i), blockColIndex(j)))
      .map(matrix => matrix(subMatrixRowIndex(i), subMatrixColIndex(j)))
      .headOption.getOrElse(0d)
  }

  def forallWithIndex(p: (Double, Long, Long) => Boolean): Boolean = {
    blocks
      .treeAggregate(true)((acc, matrixBlock) => acc && matrixBlock._2.forallWithIndex((v, subMatrixRowIndex, subMatrixColIndex) => {
        p(v, row(matrixBlock._1._1, subMatrixRowIndex), col(matrixBlock._1._2, subMatrixColIndex))
      }), _ && _)
  }

  def count(p: (Double) => Boolean): Long = {
    blocks
      .treeAggregate(0L)((acc, matrixBlock) => acc + matrixBlock._2.count(p), _ + _)
  }

  def rowSums: BlockVector = {
    // use treeAggregate approach here?
    val colBlocks: RDD[VectorBlock] = blocks
      .map(matrixBlock => (matrixBlock._1._1, matrixBlock._2))
      .aggregateByKey(Option.empty[DenseMatrix])((accCol, matrix) => {
        val newRowSums = matrix.rowSums
        accCol.map(_ + newRowSums).orElse(Some(newRowSums))
      }, (col1, col2) => {
        (col1 ++ col2).reduceOption(_ + _)
      })
      .flatMap(x => {
        val (blockRowIndex, optionalMatrix) = x
        optionalMatrix.map(matrix => (blockRowIndex, matrix))
      })

    BlockVector(colBlocks, matrix.rowsPerBlock)
  }

  def mkString: String = matrix.toLocalMatrix().toString()

  // make private stuff accessible again
  private def blocks: RDD[MatrixBlock] = matrix.blocks

  private def rowsPerBlock: Int = matrix.rowsPerBlock

  private def colsPerBlock: Int = matrix.colsPerBlock

}

object RichBlockMatrix {

  type MatrixBlock = ((Int, Int), Matrix)

  implicit def blockMatrixToRichBlockMatrix(matrix: BlockMatrix): RichBlockMatrix = new RichBlockMatrix(matrix)

}
