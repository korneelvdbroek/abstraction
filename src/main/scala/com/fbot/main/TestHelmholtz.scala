package com.fbot.main

import java.util.Random

import breeze.linalg.max
import breeze.numerics.abs
import ch.qos.logback.classic.{Level, Logger}
import com.fbot.algos.clustering.{HelmholtzClustering, MISimilarity}
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
    LoggerFactory.getLogger("org.apache.spark.scheduler.TaskSetManager").asInstanceOf[Logger].setLevel(Level.ERROR)


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
    val data: MultiSeries = {
      //InputDataGaussian().data
      //InputDataYeast().data
      InputDataUKPowerData().data
    }

    val N = data.length
    val Nclusters = 20

    // low temp => each its own
    // high temp => 1 cluster
    val temp = 1d / 100d
    val blockSizeN = N / 8
    val blockSizeNc = Nclusters


    val s = if (true) {
      val miSimilarity = MISimilarity(data, blockSizeN, blockSizeNc)
      val s = miSimilarity.similarityMatrix
      info(s"S = \n${s.mkString }")
      s.save("data/", "UKPower")
      s
    } else {
      RichBlockMatrix.load("data/UKPower")
    }


    val helmholtz = HelmholtzClustering(s, temp, blockSizeN, blockSizeNc)

    @tailrec
    def loopie(qci: BlockMatrix, iteration: Int): (BlockMatrix, Int) = {
      val qciUpdated = helmholtz.singlePass(qci)

      //      info(s"${iteration } iterations")
      //      info(s"Qci = \n${qciUpdated.mkString }")

      val diff = qciUpdated.subtract(qci)
      if (diff.fold(abs(diff(0L, 0L)))((a, b) => max(abs(a), abs(b))) < 0.01) {
        (qciUpdated, iteration)
      } else {
        loopie(qciUpdated, iteration + 1)
      }
    }

    // in future, iterations should be parallelized (either one big matrix OR loop parallelized)
    val (qci, _) = (0 until 10).toArray.foldLeft((DenseMatrix.zeros(N, Nclusters): BlockMatrix, Double.MinValue))((oldQciAndF, i) => {
      // Symmetry breaking
      val QciInit: BlockMatrix = DenseMatrix.rand(N, Nclusters, new Random)
      val (qci, iterations) = loopie(QciInit.normalizeByRow, 0)

      val aveS = helmholtz.aveS(qci)
      val mici = helmholtz.mici(qci)
      val F = aveS - temp * mici

      println(f"$i%3d, $F%8f, $aveS%8f, $temp%8f, $mici%8f, $iterations iterations")

      // Maximize F
      if (F > oldQciAndF._2) {
        (qci, F)
      } else {
        oldQciAndF
      }
    })

    val printableMatrix = qci.toLocalMatrix.toImmutableArray
    printableMatrix.toList.foreach(row => {
      print("[")
      row.toList.foreach(value => {
        print(f"$value%5.3f, ")
      })
      print("]\n")
    })
    println()
    println(f"F = ${helmholtz.freeEnergy(qci) }%8f ")
    println(f"  = <S> - T x MI(C;i) = ${helmholtz.aveS(qci) }%8f - $temp x ${helmholtz.mici(qci) }%8f ")


    // get rid of annoying ERROR messages when spark-submit shuts down
    LoggerFactory.getLogger("org.apache.spark.SparkEnv").asInstanceOf[Logger].setLevel(Level.OFF)
    LoggerFactory.getLogger("org.apache.spark.util.ShutdownHookManager").asInstanceOf[Logger].setLevel(Level.OFF)
    sc.stop()
  }
}