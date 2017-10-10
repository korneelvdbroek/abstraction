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


  def rowSums: BlockVector = {
    // use treeAggregate approach here?
    val colBlocks: RDD[VectorBlock] = matrix.blocks
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


}

object RichBlockMatrix {

  type MatrixBlock = ((Int, Int), Matrix)

  implicit def blockMatrixToRichBlockMatrix(matrix: BlockMatrix): RichBlockMatrix = new RichBlockMatrix(matrix)

}
