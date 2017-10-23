package com.fbot.main

import java.util.Random

import breeze.linalg.{max, DenseMatrix => BDM, DenseVector => BDV, Matrix => BM}
import breeze.numerics.abs
import ch.qos.logback.classic.{Level, Logger}
import com.fbot.algos.clustering.HelmholtzClustering
import com.fbot.common.data.{IndexedSeries, MultiSeries, Series}
import com.fbot.common.fastcollections.ImmutableArray
import com.fbot.common.fastcollections.index.ArrayIndex
import com.fbot.common.hyperspace.Tuple
import com.fbot.common.linalg.RichDenseMatrix._
import com.fbot.common.linalg.distributed.RichBlockMatrix
import com.fbot.common.linalg.distributed.RichBlockMatrix._
import grizzled.slf4j.Logging
import org.apache.spark.mllib.linalg.distributed.BlockMatrix
import org.apache.spark.mllib.linalg.{DenseMatrix, Matrix, SparseMatrix}
import org.apache.spark.{SparkConf, SparkContext}
import org.slf4j.LoggerFactory

import scala.annotation.tailrec
import scala.collection.mutable

/**
  *
  */
object TestHelmholtz extends Logging {

  def main(args: Array[String]): Unit = {
    LoggerFactory.getLogger(org.slf4j.Logger.ROOT_LOGGER_NAME).asInstanceOf[Logger].setLevel(Level.WARN)
    LoggerFactory.getLogger("com.fbot.main.TestHelmholtz").asInstanceOf[Logger].setLevel(Level.INFO)


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
                                   classOf[SparseMatrix],
                                   classOf[breeze.linalg.DenseMatrix$mcD$sp],
                                   classOf[Array[ArrayIndex]]))
    implicit val sc = new SparkContext(conf)


    // Data source
    val data: MultiSeries = if (false) {
      val rho: Double = 0.0d
      val Nsample: Int = 10000
      // should be > 10^{2 seriesDim}
      val seriesDim: Int = 2
      val N = 4

      val mu = BDV(Array.fill(N * seriesDim)(0d))
      val sigma = {
        val dim = 4

        val sigmaX = BDM.eye[Double](dim) + BDM((0.0, 0.0, 0.4, 0.4), (0.0, 0.0, 0.4, 0.4), (0.4, 0.4, 0.0, 0.0), (0.4, 0.4, 0.0, 0.0))
        val sigmaY = BDM.eye[Double](dim) + BDM((0.0, 0.0, 0.4, 0.4), (0.0, 0.0, 0.4, 0.4), (0.4, 0.4, 0.0, 0.0), (0.4, 0.4, 0.0, 0.0))
        val sigmaXY = BDM.eye[Double](dim) * rho //+ BDM((0.4, 0.0, -0.3, 0.0), (0.0, 0.0, 0.0, 0.0), (0.5, 0.0, 0.4, 0.0), (0.1, 0.0, 0.2, 0.0))

        val a = BDM.zeros[Double](2 * dim, 2 * dim)
        a(0 until dim, 0 until dim) := sigmaX
        a(dim until 2 * dim, dim until 2 * dim) := sigmaY
        a(0 until dim, dim until 2 * dim) := sigmaXY
        a(dim until 2 * dim, 0 until dim) := sigmaXY.t

        a
      }

      info(s"sigma = \n$sigma")
      info(s"parallelism = ${sc.defaultParallelism }")
      GaussianData(N, Nsample, seriesDim, sigma, mu)
        .data // GaussianData.data // RndDataXd(dim, N).data // FxDataXd(dim, N).data // GaussianData2d(N, rho).data
    } else {
      InputDataYeast().data
    }

    val N = data.length
    val Nclusters = 20

    // low temp => each its own
    // high temp => 1 cluster
    val temp = 1d / 35d
    val helmholtz = HelmholtzClustering(data, temp, N, Nclusters)



//    val s = helmholtz.similarityMatrix
//    info(s"S = \n${s.mkString }")
//    s.save("data/")

    val s = RichBlockMatrix.load("data/")

    @tailrec
    def loopie(qci: BlockMatrix, iteration: Int): (BlockMatrix, Int) = {
      val qciUpdated = helmholtz.singlePass(s, qci)

      info(s"${iteration } iterations")
      info(s"Qci = \n${qciUpdated.mkString }")

      val diff = qciUpdated.subtract(qci)
      if (diff.fold(abs(diff(0L, 0L)))((a, b) => max(abs(a), abs(b))) < 0.01) {
        (qciUpdated, iteration)
      } else {
        loopie(qciUpdated, iteration + 1)
      }
    }

    val QciInit: BlockMatrix = DenseMatrix.rand(N, Nclusters, new Random)
    val result = loopie(QciInit.normalizeByRow, 0)


    val printableMatrix = result._1.toLocalMatrix.toImmutableArray

    info(s"${result._2 } iterations")
    printableMatrix.toList.foreach(row => {
      print("[")
      row.toList.foreach(value =>{
        print(f"$value%5.3f, ")
      })
      print("]\n")
    })


    // get rid of annoying ERROR messages when spark-submit shuts down
    LoggerFactory.getLogger("org.apache.spark.SparkEnv").asInstanceOf[Logger].setLevel(Level.OFF)
    LoggerFactory.getLogger("org.apache.spark.util.ShutdownHookManager").asInstanceOf[Logger].setLevel(Level.OFF)
    sc.stop()
  }
}