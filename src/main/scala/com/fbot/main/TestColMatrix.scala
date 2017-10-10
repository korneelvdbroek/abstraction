package com.fbot.main

import com.fbot.common.linalg.distributed.RichBlockMatrix._
import grizzled.slf4j.Logging
import org.apache.spark.mllib.linalg.distributed.BlockMatrix
import org.apache.spark.mllib.linalg.{DenseMatrix, Matrix}
import org.apache.spark.rdd.RDD
import org.apache.spark.sql.SparkSession
import com.fbot.common.linalg.RichDenseMatrix._

/**
  *
  */
object TestColMatrix extends Logging {

  def main(args: Array[String]): Unit = {
    val spark = SparkSession.builder.appName("Simple Application").getOrCreate()
    implicit val sc = spark.sparkContext


    val A = new DenseMatrix(3, 2, Array(1, 2, 3, 4, 5, 6))
    println(s"A.forallWithIndex((v, i, j) => v > 4) = ${A.forallWithIndex((v, i, j) => if (v == 4) (i==0 && j==1) else true)}")
    println(s"A.count(_ > 4) = ${A.count(_ > 4)}")


    // Create a BlockMatrix from an RDD of sub-matrix blocks.
    val blocks: RDD[((Int, Int), Matrix)] = sc.parallelize(Seq(((0, 0), new DenseMatrix(3, 2, Array(1, 2, 3, 4, 5, 6))),
                                                               ((0, 1), new DenseMatrix(3, 2, Array(1, 2, 3, 4, 5, 6))),
                                                               ((2, 2), new DenseMatrix(3, 2, Array(7, 8, 9, 10, 11, 12)))))

    val matrix = new BlockMatrix(blocks, 3, 2).cache()

    println(s"${matrix.toLocalMatrix() }")
    println()
    println(s"${matrix.rowSums.toLocalVector }")
    println(s"matrix(0,0) = ${matrix(0,0)}")
    println(s"matrix(1,1) = ${matrix(1,1)}")
    println(s"matrix(2,2) = ${matrix(2,2)}")
    println(s"matrix(3,3) = ${matrix(3,3)}")
    println(s"matrix.count(_ > 10) = ${matrix.count(_ > 10) }")

  }

}