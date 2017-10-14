package com.fbot.common.linalg

import org.apache.spark.mllib.linalg.{DenseMatrix, Matrix, SparseMatrix}

/**
  *
  */
class RichDenseMatrix(val matrix: DenseMatrix) extends AnyVal {

  // additional matrix operations
  def rowSums: DenseMatrix = {
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

  def diagMultiply(other: DenseMatrix): DenseMatrix = {
    // TODO: cleanup this mess with macros without hurting speed...
    require(numCols == other.numRows, s"Cannot multiply: the columns of A don't match the rows of B. A: $numCols, B: ${other.numRows})")
    require(numRows == other.numCols, s"Cannot get diagonal: the rows of A don't match the columns of B. A: $numRows, B: ${other.numCols})")

    val rowValues = new Array[Double](numRows)

    (matrix.isTransposed, other.isTransposed) match {
      case (false, false) =>
        var i = 0
        while (i < numRows) {
          var k = 0
          var element = 0d
          while (k < numCols) {
            element += matrix.values(i + numRows * k) * other.values(k + numRows * i)
            k += 1
          }
          rowValues(i) = element
          i += 1
        }
      case (false, true) =>
        var i = 0
        while (i < numRows) {
          var k = 0
          var element = 0d
          while (k < numCols) {
            element += matrix.values(i + numRows * k) * other.values(i + numCols * k)
            k += 1
          }
          rowValues(i) = element
          i += 1
        }
      case (true, false) =>
        var i = 0
        while (i < numRows) {
          var k = 0
          var element = 0d
          while (k < numCols) {
            element += matrix.values(k + numCols * i) * other.values(k + numRows * i)
            k += 1
          }
          rowValues(i) = element
          i += 1
        }
      case (true, true) =>
        var i = 0
        while (i < numRows) {
          var k = 0
          var element = 0d
          while (k < numCols) {
            element += matrix.values(k + numCols * i) * other.values(i + numCols * k)
            k += 1
          }
          rowValues(i) = element
          i += 1
        }
    }

    new DenseMatrix(1, numRows, rowValues)
  }


  def toMatrix: Matrix = matrix

  // arithmetic
  def + (other: DenseMatrix): DenseMatrix = {
    require(numRows == other.numRows && numCols == other.numCols,
            s"Matrices cannot be added since dimensions (${numRows}x$numCols vs ${other.numRows}x${other.numCols}) do not match.")

    val resultValues = new Array[Double](numRows * numCols)
    if (!other.isTransposed) {
      foreachActive { (i, j, value) =>
        resultValues(i + numRows * j) = value + other.values(i + other.numRows * j)
      }
    } else {
      foreachActive { (i, j, value) =>
        resultValues(i + numRows * j) = value + other.values(j + other.numCols * i)
      }
    }
    new DenseMatrix(numRows, numCols, resultValues)
  }


  // logical tests on matrix
  def forall(p: (Double) => Boolean): Boolean = {
    val len = values.length
    var i = 0
    while (i < len && p(values(i))) {
      i += 1
    }
    i == len
  }

  def forallWithIndex(p: (Double, Int, Int) => Boolean): Boolean = {
    var i = 0
    var j = 0
    var forall = true
    if (!isTransposed) {
      // outer loop over columns
      while (j < numCols && forall) {
        i = 0
        val indStart = j * numRows
        while (i < numRows && forall) {
          forall = p(values(indStart + i), i, j)
          i += 1
        }
        j += 1
      }
    } else {
      // outer loop over rows
      while (i < numRows && forall) {
        j = 0
        val indStart = i * numCols
        while (j < numCols && forall) {
          forall = p(values(indStart + j), i, j)
          j += 1
        }
        i += 1
      }
    }
    forall
  }

  def count(p: (Double) => Boolean): Int = {
    var cnt = 0
    for (x <- values)
      if (p(x)) cnt += 1

    cnt
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


object RichDenseMatrix {

  implicit def denseMatrixToRichDenseMatrix(matrix: DenseMatrix): RichDenseMatrix = new RichDenseMatrix(matrix)


  // no optimized routines for SparseMatrix yet: we just convert to DenseMatrix
  implicit def matrixToDenseMatrix(matrix: Matrix): RichDenseMatrix = matrix match {
    case m: SparseMatrix => m.toDense
    case m: DenseMatrix => m
  }

}

