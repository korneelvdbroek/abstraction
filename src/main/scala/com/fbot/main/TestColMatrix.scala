package com.fbot.main

import breeze.linalg.{DenseMatrix => BDM, DenseVector => BDV}
import ch.qos.logback.classic.{Level, Logger}
import com.fbot.algos.clustering.HelmholtzClustering
import com.fbot.common.data.{IndexedSeries, MultiSeries, Series}
import com.fbot.common.fastcollections.ImmutableArray
import com.fbot.common.fastcollections.index.ArrayIndex
import com.fbot.common.hyperspace.Tuple
import com.fbot.common.linalg.distributed.RichBlockMatrix._
import grizzled.slf4j.Logging
import org.apache.spark.mllib.linalg.{DenseMatrix, Matrix, SparseMatrix}
import org.apache.spark.{SparkConf, SparkContext}
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
    conf.registerKryoClasses(Array(classOf[mutable.WrappedArray.ofRef[_]],
                                   classOf[mutable.WrappedArray.ofDouble],
                                   classOf[DenseMatrix],
                                   classOf[Array[Matrix]],
                                   classOf[ArrayIndex],
                                   classOf[ImmutableArray[Double]],
                                   classOf[Array[Tuple]],
                                   classOf[Tuple],
                                   classOf[IndexedSeries],
                                   classOf[Series],
                                   classOf[MultiSeries.SeriesIndexCombination],
                                   classOf[SparseMatrix]))
    implicit val sc = new SparkContext(conf)


    // Data source
    val rho: Double = 0.0d
    val N: Int = 500000
    // should be > 10^{2 seriesDim}
    val seriesDim: Int = 2
    val numberOfSeries = 4


    val mu = BDV(Array.fill(numberOfSeries * seriesDim)(0d))
    val sigma = {
      val dim = 4

      val sigmaX = BDM.eye[Double](dim)
      val sigmaY = BDM.eye[Double](dim) + BDM((0.0, 0.2, 0.0, 0.0), (0.2, 0.0, 0.0, 0.0), (0.0, 0.0, 0.0, 0.0), (0.0, 0.0, 0.0, 0.0))
      val sigmaXY = BDM.eye[Double](dim) * rho + BDM((0.4, 0.0, -0.3, 0.0), (0.0, 0.0, 0.0, 0.0), (0.5, 0.0, 0.4, 0.0), (0.1, 0.0, 0.2, 0.0))

      val a = BDM.zeros[Double](2 * dim, 2 * dim)
      a(0 until dim, 0 until dim) := sigmaX
      a(dim until 2 * dim, dim until 2 * dim) := sigmaY
      a(0 until dim, dim until 2 * dim) := sigmaXY
      a(dim until 2 * dim, 0 until dim) := sigmaXY.t

      a
    }

    info(s"sigma = $sigma")
    info(s"parallelism = ${sc.defaultParallelism }")
    val data: MultiSeries = GaussianData(numberOfSeries, N, seriesDim, sigma, mu)
      .data // GaussianData.data // RndDataXd(dim, N).data // FxDataXd(dim, N).data // GaussianData2d(N, rho).data

    info(HelmholtzClustering(data).similarityMatrix.mkString)

    // get rid of annoying ERROR messages when spark-submit shuts down
    LoggerFactory.getLogger("org.apache.spark.SparkEnv").asInstanceOf[Logger].setLevel(Level.OFF)
    LoggerFactory.getLogger("org.apache.spark.util.ShutdownHookManager").asInstanceOf[Logger].setLevel(Level.OFF)
    sc.stop()
  }
}