package com.fbot.common.linalg.distributed

import java.io.{File, PrintWriter}

import com.fbot.common.linalg.RichDenseMatrix._
import com.fbot.common.linalg.distributed.RichBlockMatrix.MatrixBlock
import org.apache.spark.{Partitioner, SparkContext}
import org.apache.spark.mllib.linalg.distributed.BlockMatrix
import org.apache.spark.mllib.linalg.{DenseMatrix, Matrix, SparseMatrix}
import org.apache.spark.rdd.RDD

import scala.io.Source
import scala.util.Try

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

  def fold(zeroValue: Double)(f: (Double, Double) => Double): Double = {
    blocks.treeAggregate(zeroValue)((acc, matrix) => {
      f(acc, matrix._2.fold(zeroValue)(f))
    }, f)
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

  /**
    * If an entire row is zero, then a zero row is returned.
    *
    * @return
    */
  def normalizeByRow: BlockMatrix = {
    val resultPartitioner = GridPartitioner(matrix.numRowBlocks, matrix.numColBlocks, suggestedNumPartitions = blocks.partitions.length)

    val blocksByBlockRowIndex = blocks
      .map(matrixBlock => (matrixBlock._1._1, matrixBlock))

    val rowSumsByBlockRowIndex: RDD[(Int, Option[DenseMatrix])] = blocksByBlockRowIndex
      .aggregateByKey(Option.empty[DenseMatrix])((accCol, matrix) => {
        val newRowSums = matrix._2.rowSums
        accCol.map(_ + newRowSums).orElse(Some(newRowSums))
      }, (col1, col2) => {
        (col1 ++ col2).reduceOption(_ + _)
      })

    val normalizedBlocks = blocksByBlockRowIndex
      .join(rowSumsByBlockRowIndex, resultPartitioner).map(x => {
      val originalMatrixBlock = x._2._1
      val normalizationVector = x._2._2.get

      val normalizedMatrix = originalMatrixBlock._2.mapWithIndex((i, _, value) => {
        if (normalizationVector(i, 0) == 0) throw new IllegalArgumentException("division by zero...")
        value / normalizationVector(i, 0)
      })

      (originalMatrixBlock._1, normalizedMatrix.toMatrix)
    })

    new BlockMatrix(normalizedBlocks, matrix.rowsPerBlock, matrix.colsPerBlock)
  }

  /**
    * If an entire row is zero, then a zero row is returned.
    *
    * @return
    */
  def normalizeByCol: BlockMatrix = {
    val resultPartitioner = GridPartitioner(matrix.numRowBlocks, matrix.numColBlocks, suggestedNumPartitions = blocks.partitions.length)

    val blocksByBlockColIndex = blocks
      .map(matrixBlock => (matrixBlock._1._2, matrixBlock))

    val colSumsByBlockColIndex: RDD[(Int, Option[DenseMatrix])] = blocksByBlockColIndex
      .aggregateByKey(Option.empty[DenseMatrix])((accRow, matrix) => {
        val newColSums = matrix._2.colSums
        accRow.map(_ + newColSums).orElse(Some(newColSums))
      }, (col1, col2) => {
        (col1 ++ col2).reduceOption(_ + _)
      })

    val normalizedBlocks = blocksByBlockColIndex
      .join(colSumsByBlockColIndex, resultPartitioner).map(x => {
      val originalMatrixBlock = x._2._1
      val normalizationVector = x._2._2.get

      val normalizedMatrix = originalMatrixBlock._2.mapWithIndex((_, j, value) => {
        if (normalizationVector(0, j) == 0) throw new IllegalArgumentException("division by zero...")
        value / normalizationVector(0, j)
      })

      (originalMatrixBlock._1, normalizedMatrix.toMatrix)
    })

    new BlockMatrix(normalizedBlocks, matrix.rowsPerBlock, matrix.colsPerBlock)
  }


  def rowSums: BlockMatrix = {
    // use treeAggregate approach here?
    val colBlocks: RDD[MatrixBlock] = blocks
      .map(matrixBlock => (matrixBlock._1._1, matrixBlock._2))
      .aggregateByKey(Option.empty[DenseMatrix])((accCol, matrix) => {
        val newRowSums = matrix.rowSums
        accCol.map(_ + newRowSums).orElse(Some(newRowSums))
      }, (col1, col2) => {
        (col1 ++ col2).reduceOption(_ + _)
      })
      .flatMap(x => {
        val (blockRowIndex, optionalMatrix) = x
        optionalMatrix.map(matrix => ((blockRowIndex, 0), matrix))
      })

    new BlockMatrix(colBlocks, matrix.rowsPerBlock, 1)
  }

  def colSums: BlockMatrix = {
    val colBlocks: RDD[MatrixBlock] = blocks
      .map(matrixBlock => (matrixBlock._1._2, matrixBlock._2))
      .aggregateByKey(Option.empty[DenseMatrix])((accColSum, matrix) => {
        val newColSums = matrix.colSums
        accColSum.map(_ + newColSums).orElse(Some(newColSums))
      }, (col1, col2) => {
        (col1 ++ col2).reduceOption(_ + _)
      })
      .flatMap(x => {
        val (blockColIndex, optionalMatrix) = x
        optionalMatrix.map(matrix => ((0, blockColIndex), matrix))
      })

    new BlockMatrix(colBlocks, 1, matrix.colsPerBlock)
  }


  def map(f: Double => Double): BlockMatrix = {
    val mappedBlocks: RDD[MatrixBlock] = blocks.map(matrixBlock => {
      (matrixBlock._1, matrixBlock._2.map(f))
    })

    new BlockMatrix(mappedBlocks, matrix.rowsPerBlock, matrix.colsPerBlock)
  }


  def mapWithIndex(f: (Long, Long, Double) => Double): BlockMatrix = {
    val mappedBlocks: RDD[MatrixBlock] = blocks.map(matrixBlock => {
      val (blockRowIndex, blockColIndex) = matrixBlock._1

      val mappedMatrix: Matrix = matrixBlock._2.mapWithIndex((subMatrixRowIndex, subMatrixColIndex, value) => {
        f(row(blockRowIndex, subMatrixRowIndex), col(blockColIndex, subMatrixColIndex), value)
      })

      ((blockRowIndex, blockColIndex), mappedMatrix)
    })

    new BlockMatrix(mappedBlocks, matrix.rowsPerBlock, matrix.colsPerBlock)
  }


  /**
    * Simulate the multiplication with just block indices in order to cut costs on communication,
    * when we are actually shuffling the matrices.
    * The `colsPerBlock` of this matrix must equal the `rowsPerBlock` of `other`.
    * Exposed for tests.
    *
    * @param other       The BlockMatrix to multiply
    * @param partitioner The partitioner that will be used for the resulting matrix `C = A * B`
    * @return A tuple of [[BlockDestination]]. The first element is the Map of the set of partitions
    *         that we need to shuffle each blocks of `this`, and the second element is the Map for
    *         `other`.
    */
  def simulateDiagMultiply(other: BlockMatrix,
                           partitioner: GridPartitioner,
                           midDimSplitNum: Int): (BlockDestination, BlockDestination) = {
    val leftMatrix = blockInfo.keys.collect()
    val rightMatrix = new RichBlockMatrix(other).blockInfo.keys.collect()

    val leftDestinations = leftMatrix.map { case (rowIndex, colIndex) =>
      val pid = partitioner.getPartition((0, rowIndex))
      val midDimSplitIndex = colIndex % midDimSplitNum
      ((rowIndex, colIndex),
        pid * midDimSplitNum + midDimSplitIndex)
    }.toMap

    val rightDestinations = rightMatrix.map { case (rowIndex, colIndex) =>
      val pid = partitioner.getPartition((0, colIndex))
      val midDimSplitIndex = rowIndex % midDimSplitNum
      ((rowIndex, colIndex),
        pid * midDimSplitNum + midDimSplitIndex)
    }.toMap

    (leftDestinations, rightDestinations)
  }

  /**
    * Left multiplies this [[BlockMatrix]] to `other`, another [[BlockMatrix]]. The `colsPerBlock`
    * of this matrix must equal the `rowsPerBlock` of `other`. If `other` contains
    * `SparseMatrix`, they will have to be converted to a `DenseMatrix`. The output
    * [[BlockMatrix]] will only consist of blocks of `DenseMatrix`. This may cause
    * some performance issues until support for multiplying two sparse matrices is added.
    * Blocks with duplicate indices will be added with each other.
    *
    * @param other           Matrix `B` in `A * B = C`
    * @param numMidDimSplits Number of splits to cut on the middle dimension when doing
    *                        multiplication. For example, when multiplying a Matrix `A` of
    *                        size `m x n` with Matrix `B` of size `n x k`, this parameter
    *                        configures the parallelism to use when grouping the matrices. The
    *                        parallelism will increase from `m x k` to `m x k x numMidDimSplits`,
    *                        which in some cases also reduces total shuffled data.
    */
  def diagMultiply(other: BlockMatrix,
                   numMidDimSplits: Int = 1): BlockMatrix = {
    require(matrix.numCols == other.numRows(), "The number of columns of A and the number of rows " +
                                               s"of B must be equal. A.numCols: ${matrix.numCols }, B.numRows: ${other.numRows() }. If you " +
                                               "think they should be equal, try setting the dimensions of A and B explicitly while " +
                                               "initializing them.")
    require(numMidDimSplits > 0, "numMidDimSplits should be a positive integer.")

    if (colsPerBlock == other.rowsPerBlock) {
      val resultPartitioner = GridPartitioner(1, other.numColBlocks,
                                              math.max(blocks.partitions.length, other.blocks.partitions.length))
      val (leftDestinations, rightDestinations) = simulateDiagMultiply(other, resultPartitioner, numMidDimSplits)
      // Each block of A must be multiplied with the corresponding blocks in the columns of B.
      val flatA = blocks.flatMap { case ((blockRowIndex, blockColIndex), block) =>
        val destination = leftDestinations.get((blockRowIndex, blockColIndex))
        destination.map(j => (j, (blockRowIndex, blockColIndex, block)))
      }
      // Each block of B must be multiplied with the corresponding blocks in each row of A.
      val flatB = other.blocks.flatMap { case ((blockRowIndex, blockColIndex), block) =>
        val destination = rightDestinations.get((blockRowIndex, blockColIndex))
        destination.map(j => (j, (blockRowIndex, blockColIndex, block)))
      }
      val intermediatePartitioner = new Partitioner {
        override def numPartitions: Int = resultPartitioner.numPartitions * numMidDimSplits

        override def getPartition(key: Any): Int = key.asInstanceOf[Int]
      }
      val newBlocks = flatA.cogroup(flatB, intermediatePartitioner).flatMap { case (_, (a, b)) =>
        a.flatMap { case (_, leftColIndex, leftBlock) =>
          b.filter(_._1 == leftColIndex).map { case (_, rightColIndex, rightBlock) =>
            val C = rightBlock match {
              case dense: DenseMatrix => leftBlock.diagMultiply(dense)
              case sparse: SparseMatrix => leftBlock.diagMultiply(sparse.toDense)
              case _ =>
                throw new IllegalArgumentException(s"Unrecognized matrix type ${rightBlock.getClass }.")
            }
            ((0, rightColIndex), C)
          }
        }
      }.reduceByKey(resultPartitioner, (a, b) => a + b).mapValues(_.toMatrix)
      // TODO: Try to use aggregateByKey instead of reduceByKey to get rid of intermediate matrices
      new BlockMatrix(newBlocks, 1, other.colsPerBlock, 1, other.numCols())
    } else {
      throw new IllegalArgumentException("colsPerBlock of A doesn't match rowsPerBlock of B. " +
                                         s"A.colsPerBlock: $colsPerBlock, B.rowsPerBlock: ${other.rowsPerBlock }")
    }
  }


  def save(path: String, prefix: String = ""): Unit = {
    val writer = new PrintWriter(new File(s"$path/$prefix${ RichBlockMatrix.metaDataFileName }"))
    writer.write(s"${matrix.rowsPerBlock}\n")
    writer.write(s"${matrix.colsPerBlock}\n")
    writer.write(s"${matrix.numRows()}\n")
    writer.write(s"${matrix.numCols()}\n")
    writer.close()
    matrix.blocks.saveAsObjectFile(s"$path/$prefix${ RichBlockMatrix.dataDirectory }")
  }


  def mkString: String = matrix.toLocalMatrix().toString()


  // make private stuff accessible again
  /** Block (i,j) --> Set of destination partitions */
  private type BlockDestination = Map[(Int, Int), Int]

  def blockInfo: RDD[((Int, Int), (Int, Int))] = blocks.mapValues(block => (block.numRows, block.numCols)).cache()

  private def blocks: RDD[MatrixBlock] = matrix.blocks

  private def rowsPerBlock: Int = matrix.rowsPerBlock

  private def colsPerBlock: Int = matrix.colsPerBlock

}

object RichBlockMatrix {

  val metaDataFileName = "blockMatrixMetaData.dat"
  val dataDirectory = "blockMatrixData"

  type MatrixBlock = ((Int, Int), Matrix)

  implicit def blockMatrixToRichBlockMatrix(matrix: BlockMatrix): RichBlockMatrix = new RichBlockMatrix(matrix)

  implicit def denseMatrixToBlockMatrix(matrix: DenseMatrix)(implicit sc: SparkContext): BlockMatrix = {
    val rdd = sc.parallelize(Seq(((0, 0), matrix.toMatrix)))
    new BlockMatrix(rdd, matrix.numRows, matrix.numCols)
  }

  def load(path: String, prefix: String)(implicit sc: SparkContext): BlockMatrix = {
    val bufferedSource = Source.fromFile(s"$path/$prefix$metaDataFileName")
    val blockMatrixData = bufferedSource.getLines
    val rowsPerBlock: Int = Try(blockMatrixData.next().toInt).getOrElse(throw new IllegalArgumentException(s"Cannot read rowsPerBlock from file $path/$prefix$metaDataFileName"))
    val colsPerBlock: Int = Try(blockMatrixData.next().toInt).getOrElse(throw new IllegalArgumentException(s"Cannot read colsPerBlock from file $path/$prefix$metaDataFileName"))
    val nRows: Long = Try(blockMatrixData.next().toLong).getOrElse(throw new IllegalArgumentException(s"Cannot read numRows from file $path/$prefix$metaDataFileName"))
    val nCols: Long = Try(blockMatrixData.next().toLong).getOrElse(throw new IllegalArgumentException(s"Cannot read numCols from file $path/$prefix$metaDataFileName"))
    bufferedSource.close

    val blocks: RDD[MatrixBlock] = sc.objectFile(s"$path/$prefix$dataDirectory")

    new BlockMatrix(blocks, rowsPerBlock, colsPerBlock, nRows, nCols)
  }

}
