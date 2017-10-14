package com.fbot.common.linalg.distributed

import org.apache.spark.mllib.linalg.distributed.BlockMatrix
import org.apache.spark.mllib.linalg.{DenseMatrix, Matrix}
import org.apache.spark.rdd.RDD
import org.apache.spark.{SparkConf, SparkContext}
import org.scalatest.{BeforeAndAfter, FlatSpec, Matchers}
import RichBlockMatrix._

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
    println(A.multiply(B).mkString)
    println(A.diagMultiply(B, 1).mkString)
  }

}

object SpecRichBlockMatrix {

  val master = "local[2]"
  val appName = "example-spark"

  implicit var sc: SparkContext = _

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

}
