package com.fbot.main

import com.fbot.common.linalg.RichDenseMatrix._
import com.fbot.common.linalg.distributed.RichBlockMatrix._
import org.apache.spark.mllib.linalg.distributed.BlockMatrix
import org.apache.spark.mllib.linalg.{DenseMatrix, Matrix}
import org.apache.spark.rdd.RDD
import org.apache.spark.{SparkConf, SparkContext}
import ch.qos.logback.classic.Level
import ch.qos.logback.classic.Logger
import grizzled.slf4j.Logging
import org.slf4j.LoggerFactory

import scala.collection.mutable

/**
  *
  */
object TestColMatrix extends Logging {

  def main(args: Array[String]): Unit = {
    LoggerFactory.getLogger(org.slf4j.Logger.ROOT_LOGGER_NAME).asInstanceOf[Logger].setLevel(Level.WARN)
    LoggerFactory.getLogger("com.fbot.main.TestColMatrix").asInstanceOf[Logger].setLevel(Level.INFO)


    val conf = new SparkConf().setAppName("Simple Application")
    conf.registerKryoClasses(Array(classOf[mutable.WrappedArray.ofRef[_]], classOf[DenseMatrix], classOf[Array[Matrix]]))
    implicit val sc = new SparkContext(conf)


    val A = new DenseMatrix(3, 2, Array(1, 2, 3, 4, 5, 6))
    info(s"A.forallWithIndex((v, i, j) => v > 4) = ${A.forallWithIndex((v, i, j) => if (v == 4) (i == 0 && j == 1) else true) }")
    info(s"A.count(_ > 4) = ${A.count(_ > 4) }")


    // Create a BlockMatrix from an RDD of sub-matrix blocks.
    val blocks: RDD[((Int, Int), Matrix)] = sc.parallelize(Seq(((0, 0), new DenseMatrix(3, 2, Array(1, 2, 3, 4, 5, 6))),
                                                               ((0, 1), new DenseMatrix(3, 2, Array(1, 2, 3, 4, 5, 6))),
                                                               ((2, 2), new DenseMatrix(3, 2, Array(7, 8, 9, 10, 11, 12)))))

    val matrix = new BlockMatrix(blocks, 3, 2).cache()

    info(s"${matrix.toLocalMatrix() }")
    info(s"${matrix.rowSums.toLocalVector }")
    info(s"matrix(0,0) = ${matrix(0, 0) }")
    info(s"matrix(1,1) = ${matrix(1, 1) }")
    info(s"matrix(2,2) = ${matrix(2, 2) }")
    info(s"matrix(3,3) = ${matrix(3, 3) }")
    info(s"matrix.count(_ > 10) = ${matrix.count(_ > 10) }")



    // get rid of annoying ERROR messages when spark-submit shuts down
    LoggerFactory.getLogger("org.apache.spark.SparkEnv").asInstanceOf[Logger].setLevel(Level.OFF)
    LoggerFactory.getLogger("org.apache.spark.util.ShutdownHookManager").asInstanceOf[Logger].setLevel(Level.OFF)
    sc.stop()
  }
}