package com.fbot.main

import grizzled.slf4j.Logging
import org.apache.spark.mllib.linalg.distributed.BlockMatrix
import org.apache.spark.mllib.linalg.{DenseMatrix, Matrix, SparseMatrix}
import org.apache.spark.rdd.RDD
import org.apache.spark.sql.SparkSession

/**
  * Copyright (C) 10/3/2017 - REstore NV
  *
  */
object TestColMatrix extends Logging {

  def main(args: Array[String]): Unit = {
    val spark = SparkSession.builder.appName("Simple Application").getOrCreate()
    implicit val sc = spark.sparkContext


    // Create a BlockMatrix from an RDD of sub-matrix blocks.
    val blocks: RDD[((Int, Int), Matrix)] = sc.parallelize(Seq(((0, 0), new DenseMatrix(3, 2, Array(1, 2, 3, 4, 5, 6))),
                                                               ((2, 2), new DenseMatrix(3, 2, Array(7, 8, 9, 10, 11, 12)))))

    val matrix = new BlockMatrix(blocks, 3, 2).cache()

    println(s"${matrix.toLocalMatrix() }")
    println()
    println(s"${matrix.sumCols.toLocalMatrix() }")
  }


  implicit class RichBlockMatrix(val matrix: BlockMatrix) extends AnyVal {

    type MatrixBlock = ((Int, Int), DenseMatrix)

    def sumCols: BlockMatrix = {
      // use treeAggregate approach here?
      val colBlocks: RDD[((Int, Int), Matrix)] = matrix.blocks.map(matrixBlock => (matrixBlock._1._1, matrixBlock))
        .aggregateByKey(Option.empty[DenseMatrix])((accCol, blockMatrix) => {
          val (_, matrix: DenseMatrix) = blockMatrix
          accCol.map(_ + matrix.sumCols).orElse(Some(matrix.sumCols))
        }, (col1, col2) => {
          (col1 ++ col2).reduceOption(_ + _)
        }).flatMap(x => {
        x._2.map(matrix => ((x._1, 0), matrix))
      })

      new BlockMatrix(colBlocks, matrix.rowsPerBlock, 1)
    }


    def printPartitions: String = {
      ???
    }
  }

  implicit def matrixToDenseMatrix(matrix: Matrix): DenseMatrix = matrix match {
    case m: DenseMatrix => m
    case m: SparseMatrix => m.toDense // no high performance support for sparse matrices yet...
  }

  implicit class RichDenseMatrix(val matrix: DenseMatrix) extends AnyVal {

    def sumCols: DenseMatrix = {
      val colValues = new Array[Double](numRows)

      if (!isTransposed) {
        // copy of first column
        System.arraycopy(values, 0, colValues, 0, numRows)

        // outer loop over columns
        var j = 1
        while (j < numCols) {
          var i = 0
          val jOffset = j * numRows
          while (i < numRows) {
            colValues(i) += values(jOffset + i)
            i += 1
          }
          j += 1
        }

      } else {
        // outer loop over rows
        var i = 0
        while (i < numRows) {
          var j = 0
          val iOffset = i * numCols
          var colSum = 0d
          while (j < numCols) {
            colSum += values(iOffset + j)
            j += 1
          }
          colValues(i) = colSum
          i += 1
        }
      }

      new DenseMatrix(numRows, 1, colValues)
    }

    def + (other: DenseMatrix): DenseMatrix = {
      val resultValues = new Array[Double](numRows * numCols)
      foreachActive { (i, j, v) =>
        resultValues(i + numRows * j) = v + other(i, j)
      }

      new DenseMatrix(numRows, numCols, resultValues)
    }


    // make private stuff accessible again
    private def numRows: Int = matrix.numRows

    private def numCols: Int = matrix.numCols

    private def values: Array[Double] = matrix.values

    private def isTransposed: Boolean = matrix.isTransposed

    private def foreachActive(f: (Int, Int, Double) => Unit): Unit = {
      if (!isTransposed) {
        // outer loop over columns
        var j = 0
        while (j < numCols) {
          var i = 0
          val indStart = j * numRows
          while (i < numRows) {
            f(i, j, values(indStart + i))
            i += 1
          }
          j += 1
        }
      } else {
        // outer loop over rows
        var i = 0
        while (i < numRows) {
          var j = 0
          val indStart = i * numCols
          while (j < numCols) {
            f(i, j, values(indStart + j))
            j += 1
          }
          i += 1
        }
      }
    }
  }

}