package com.fbot.main

import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter
import java.util.Random

import breeze.linalg.max
import breeze.numerics.abs
import ch.qos.logback.classic.{Level, Logger}
import com.fbot.algos.clustering.{HelmholtzClustering, MISimilarity}
import com.fbot.common.data.{IndexedSeries, MultiSeries, Series}
import com.fbot.common.fastcollections.ImmutableArray
import com.fbot.common.fastcollections.Tuple
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
import scala.reflect.ClassTag

/**
  * Copyright (C) 2017-2018  korneelvdbroek
  *
  * This program is free software: you can redistribute it and/or modify
  * it under the terms of the GNU General Public License as published by
  * the Free Software Foundation, either version 3 of the License, or
  * (at your option) any later version.
  *
  * This program is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU General Public License for more details.
  *
  * You should have received a copy of the GNU General Public License
  * along with this program.  If not, see <http://www.gnu.org/licenses/>.
  *
  */
object TestHelmholtz extends Logging {

  def main(args: Array[String]): Unit = {
    LoggerFactory.getLogger(org.slf4j.Logger.ROOT_LOGGER_NAME).asInstanceOf[Logger].setLevel(Level.WARN)
    LoggerFactory.getLogger("com.fbot.main").asInstanceOf[Logger].setLevel(Level.INFO)
    LoggerFactory.getLogger("com.fbot.algos").asInstanceOf[Logger].setLevel(Level.INFO)
    LoggerFactory.getLogger("com.fbot.common").asInstanceOf[Logger].setLevel(Level.INFO)
    LoggerFactory.getLogger("org.apache.spark.scheduler.TaskSetManager").asInstanceOf[Logger].setLevel(Level.ERROR)


    val conf = new SparkConf().setAppName("Simple Application")
    conf.registerKryoClasses(Array(classOf[DenseMatrix],
                                   classOf[Array[Matrix]],
                                   classOf[Array[Double]],
                                   classOf[IndexedSeries],
                                   classOf[Series],
                                   classOf[MultiSeries.SeriesIndexCombination],
                                   classOf[SparseMatrix],
                                   classOf[breeze.linalg.DenseMatrix$mcD$sp],
                                   classOf[Array[Int]],
                                   ClassTag(Class.forName("org.apache.spark.util.collection.CompactBuffer")).wrap.runtimeClass))
    implicit val sc = new SparkContext(conf)


    // Data source
    val data: MultiSeries = {
      //InputDataGaussian().data
      //InputDataYeast().data
      //InputDataUKPowerData().data
      InputDataIcebergs().data
    }

    val N = data.length
    val Nclusters = 4

    // low temp => each its own
    // high temp => 1 cluster
    val temp = 0.5
    val blockSizeN = N
    val blockSizeNc = Nclusters


    val s = if (true) {
      val miSimilarity = MISimilarity(data, blockSizeN, blockSizeNc)
      val s = miSimilarity.similarityMatrix
      s.save("temp_data/", s"UKPower${ZonedDateTime.now().format(DateTimeFormatter.ofPattern("yyyyMMdd'T'HHmmss")) }")
      s
    } else {
      RichBlockMatrix.load("temp_data/", "UKPower20180103T212017")
    }
    info(s"S = \n${Utils.printImmutableMatrix(s.toLocalMatrix.toImmutableArray) }")


    val helmholtz = HelmholtzClustering(s, temp, blockSizeN, blockSizeNc)

    @tailrec
    def loopie(qci: BlockMatrix, iteration: Int): (BlockMatrix, Int) = {
      val qciUpdated = helmholtz.singlePass(qci)

      val deltaQci = qciUpdated.subtract(qci)
      val biggestDelta = deltaQci.fold(abs(deltaQci(0L, 0L)))((a, b) => max(abs(a), abs(b)))

      //      {
      //        val flatArrayWithIndex = ImmutableArray(deltaQci.toLocalMatrix().toArray).mapWithIndex((x, i) => (x, i.toInt))
      //        val maxDeltas = flatArrayWithIndex.sortBy(_._1).take(10).map(xAndIndex => {
      //          val (x, index) = xAndIndex
      //          val row = index % deltaQci.numRows()
      //          val col = index / deltaQci.numRows()
      //          (x, qciUpdated(row, col), qci(row, col), (row, col))
      //        })
      //
      //        info(f"$iteration%3d: biggest delta = $biggestDelta%4.3f")
      //        info(f"  $maxDeltas")
      //      }

      if (biggestDelta < 0.01) {
        (qciUpdated, iteration)
      } else {
        loopie(qciUpdated, iteration + 1)
      }
    }

    //TODO: iterations should be parallelized (either one big matrix OR loop parallelized)
    val (qci, _) = (0 until 10).toArray.foldLeft((DenseMatrix.zeros(N, Nclusters): BlockMatrix, Double.MinValue))((oldQciAndF, i) => {
      // Symmetry breaking
      val QciInit: BlockMatrix = DenseMatrix.rand(N, Nclusters, new Random)
      val (qci, iterations) = loopie(QciInit.normalizeByRow, 0)

      val aveS = helmholtz.aveS(qci)
      val mici = helmholtz.mici(qci)
      val F = aveS - temp * mici

      println(f"$i%3d, $F%8f = $aveS%8f - $temp%8f * $mici%8f, $iterations iterations")

      // Maximize F
      if (F > oldQciAndF._2) {
        (qci, F)
      } else {
        oldQciAndF
      }
    })

    println(Utils.printImmutableMatrix(qci.toLocalMatrix.toImmutableArray))
    println()
    println(f"F = ${helmholtz.freeEnergy(qci) }%8f ")
    println(f"  = <S> - T x MI(C;i) = ${helmholtz.aveS(qci) }%8f - $temp x ${helmholtz.mici(qci) }%8f ")


    // get rid of annoying ERROR messages when spark-submit shuts down
    LoggerFactory.getLogger("org.apache.spark.SparkEnv").asInstanceOf[Logger].setLevel(Level.OFF)
    LoggerFactory.getLogger("org.apache.spark.util.ShutdownHookManager").asInstanceOf[Logger].setLevel(Level.OFF)
    sc.stop()
  }
}