package com.fbot.common.linalg.distributed

import breeze.linalg.max
import com.fbot.common.linalg.RichDenseMatrix._
import com.fbot.common.linalg.distributed.RichBlockMatrix._
import org.apache.spark.mllib.linalg.distributed.BlockMatrix
import org.apache.spark.mllib.linalg.{DenseMatrix, Matrix}
import org.apache.spark.rdd.RDD
import org.apache.spark.{SparkConf, SparkContext}
import org.scalatest.{BeforeAndAfter, FlatSpec, Matchers}

/**
  *
  */
class SpecRichBlockMatrix extends FlatSpec with Matchers with BeforeAndAfter {

  import SpecRichBlockMatrix._

  before {
    val conf = new SparkConf()
      .setMaster(master)
      .setAppName(appName)

    sc = new SparkContext(conf)
  }

  after {
    if (sc != null) {
      sc.stop()
    }
  }


  "diagMultiply" should "return the diagonal of the multiplication of matrices A and B" in {
    A.diagMultiply(B, 1).toLocalMatrix() shouldBe new DenseMatrix(1, 4, Array(40d, 80d, 91d, 34d))
  }

  it should "return the diagonal of the multiplication of matrices A.T and B" in {
    A.transpose.diagMultiply(B, 1).toLocalMatrix() shouldBe new DenseMatrix(1, 4, Array(69d, 122d, 45d, 29d))
  }

  it should "return the diagonal of the multiplication of matrices A and B.T" in {
    A.diagMultiply(B.transpose, 1).toLocalMatrix() shouldBe new DenseMatrix(1, 4, Array(32d, 45d, 73d, 115d))
  }

  it should "return the diagonal of the multiplication of matrices A.T and B.T" in {
    A.transpose.diagMultiply(B.transpose, 1).toLocalMatrix() shouldBe new DenseMatrix(1, 4, Array(54d, 73d, 41d, 77d))
  }


  "colSums" should "sums of the columns of A into a row vector" in {
    A.colSums.toLocalMatrix() shouldBe new DenseMatrix(1, 4, Array(14d, 22d, 18d, 26d))
  }

  it should "sums of the columns of A.T into a row vector" in {
    A.transpose.colSums.toLocalMatrix() shouldBe new DenseMatrix(1, 4, Array(10d, 14d, 26d, 30d))
  }

  "rowSums" should "sums of the columns of A into a row vector" in {
    A.rowSums.toLocalMatrix() shouldBe new DenseMatrix(4, 1, Array(10d, 14d, 26d, 30d))
  }

  it should "sums of the columns of A.T into a row vector" in {
    A.transpose.rowSums.toLocalMatrix() shouldBe new DenseMatrix(4, 1, Array(14d, 22d, 18d, 26d))
  }

  "mapWithIndex" should "map the elements of A" in {
    val resultValues: Array[Double] = Array(0, 1, 2, 3,
                                            4, 5, 6, 7,
                                            8, 9, 10, 11,
                                            12, 13, 14, 15)
    A.mapWithIndex((i, j, value) => value + i + 4 * j).toLocalMatrix() shouldBe A.toLocalMatrix + new DenseMatrix(4, 4, resultValues)
  }

  "normalizeByRow" should "return the row-normalized of A" in {
    A.normalizeByRow.toLocalMatrix shouldBe new DenseMatrix(4, 4, Array(1d/10, 2d/14, 5d/26, 6d/30,
                                                                        3d/10, 4d/14, 7d/26, 8d/30,
                                                                        2d/10, 3d/14, 6d/26, 7d/30,
                                                                        4d/10, 5d/14, 8d/26, 9d/30))
  }

  it should "return the row-normalized of a matrix with missing Blocks" in {
    C.normalizeByRow.toLocalMatrix shouldBe new DenseMatrix(4, 4, Array(1d/4, 2d/6, 0d/14, 0d/16,
                                                                        3d/4, 4d/6, 0d/14, 0d/16,
                                                                        0d/4, 0d/6, 6d/14, 7d/16,
                                                                        0d/4, 0d/6, 8d/14, 9d/16))
  }

  it should "return the row-normalized of a matrix with missing rows" in {
    D.normalizeByRow.toLocalMatrix shouldBe new DenseMatrix(4, 4, Array(0d/4, 0d/6, 0d/14, 0d/16,
                                                                        0d/4, 0d/6, 0d/14, 0d/16,
                                                                        0d/4, 0d/6, 6d/14, 7d/16,
                                                                        0d/4, 0d/6, 8d/14, 9d/16))
  }

  "fold" should "return the maximum of all elements" in {
    val M = C
    M.fold(M(0L, 0L))(max(_, _)) shouldBe 9d
  }

}

object SpecRichBlockMatrix {

  val master = "local[2]"
  val appName = "example-spark"

  implicit var sc: SparkContext = _

  /**
    * A =
    *   1 3 2 4
    *   2 4 3 5
    *   5 7 6 8
    *   6 8 7 9
    */
  def A(implicit sc: SparkContext): BlockMatrix = {
    val A00 = new DenseMatrix(2, 2, Array(1, 2, 3, 4))
    val A01 = new DenseMatrix(2, 2, Array(2, 3, 4, 5))
    val A10 = new DenseMatrix(2, 2, Array(5, 6, 7, 8))
    val A11 = new DenseMatrix(2, 2, Array(6, 7, 8, 9))
    val rddA: RDD[((Int, Int), Matrix)] = sc.parallelize(Seq(((0, 0), A00), ((0, 1), A01), ((1, 0), A10), ((1, 1), A11)))
    new BlockMatrix(rddA, 2, 2)
  }

  def B(implicit sc: SparkContext): BlockMatrix = {
    val B00 = new DenseMatrix(2, 2, Array(1, 1, 3, 5))
    val B01 = new DenseMatrix(2, 2, Array(7, 6, 2, 1))
    val B10 = new DenseMatrix(2, 2, Array(6, 6, 3, 9))
    val B11 = new DenseMatrix(2, 2, Array(1, 1, 2, 0))
    val rddB: RDD[((Int, Int), Matrix)] = sc.parallelize(Seq(((0, 0), B00), ((0, 1), B01), ((1, 0), B10), ((1, 1), B11)))
    new BlockMatrix(rddB, 2, 2)
  }

  /**
    * C =
    *   1 3 0 0
    *   2 4 0 0
    *   0 0 6 8
    *   0 0 7 9
    */
  def C(implicit sc: SparkContext): BlockMatrix = {
    val C00 = new DenseMatrix(2, 2, Array(1, 2, 3, 4))
    val C11 = new DenseMatrix(2, 2, Array(6, 7, 8, 9))
    val rddC: RDD[((Int, Int), Matrix)] = sc.parallelize(Seq(((0, 0), C00), ((1, 1), C11)))
    new BlockMatrix(rddC, 2, 2)
  }

  /**
    * C =
    *   0 0 0 0
    *   0 0 0 0
    *   0 0 6 8
    *   0 0 7 9
    */
  def D(implicit sc: SparkContext): BlockMatrix = {
    val D11 = new DenseMatrix(2, 2, Array(6, 7, 8, 9))
    val rddD: RDD[((Int, Int), Matrix)] = sc.parallelize(Seq(((1, 1), D11)))
    new BlockMatrix(rddD, 2, 2)
  }
}
