package com.fbot.main

import com.fbot.common.linalg.distributed.RichBlockMatrix._
import grizzled.slf4j.Logging
import org.apache.spark.mllib.linalg.distributed.BlockMatrix
import org.apache.spark.mllib.linalg.{DenseMatrix, Matrix}
import org.apache.spark.rdd.RDD
import org.apache.spark.sql.SparkSession

/**
  *
  */
object TestColMatrix extends Logging {

  def main(args: Array[String]): Unit = {
    val spark = SparkSession.builder.appName("Simple Application").getOrCreate()
    implicit val sc = spark.sparkContext


    // Create a BlockMatrix from an RDD of sub-matrix blocks.
    val blocks: RDD[((Int, Int), Matrix)] = sc.parallelize(Seq(((0, 0), new DenseMatrix(3, 2, Array(1, 2, 3, 4, 5, 6))),
                                                               ((0, 1), new DenseMatrix(3, 2, Array(1, 2, 3, 4, 5, 6))),
                                                               ((2, 2), new DenseMatrix(3, 2, Array(7, 8, 9, 10, 11, 12)))))

    val matrix = new BlockMatrix(blocks, 3, 2).cache()

    println(s"${matrix.toLocalMatrix() }")
    println()
    println(s"${matrix.rowSums.toLocalMatrix() }")
  }

}