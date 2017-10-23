package com.fbot.algos.clustering

import breeze.numerics.exp
import com.fbot.algos.mutualinformation.MutualInformation
import com.fbot.common.data.MultiSeries
import com.fbot.common.data.MultiSeries.SeriesIndexCombination
import com.fbot.common.fastcollections.index.ArrayIndex
import com.fbot.common.linalg.distributed.RichBlockMatrix._
import org.apache.spark.SparkContext
import org.apache.spark.mllib.linalg.distributed.{BlockMatrix, CoordinateMatrix, MatrixEntry}

/**
  *
  */
case class HelmholtzClustering(data: MultiSeries, temp: Double, blockSizeN: Int, blockSizeNc: Int)(implicit sc: SparkContext) {


  def singlePass(s: BlockMatrix, qci: BlockMatrix): BlockMatrix = {
    val N: Int = data.length
    val beta: Double = 1d / temp

    // 1 x Nc:
    val qc = sc.broadcast(qci.colSums.toLocalMatrix)
    // 1 x Nc:
    val sCluster = sc.broadcast(qci.transpose.multiply(s).diagMultiply(qci).toLocalMatrix)
    val sci = s.multiply(qci)

    val qciUpdated = sci.mapWithIndex((_, j, sciValue) => {
      val qcValue = qc.value(0, j.toInt) / N

      if (qcValue == 0.0) throw new IllegalArgumentException("Qc = zero...")
      if (qcValue.isNaN) throw new IllegalArgumentException("Naan...")

      val x = qcValue * exp(beta * (sciValue * 2d / N / qcValue - sCluster.value(0, j.toInt) / (N * N * qcValue * qcValue)))

      println(f"$x%f  =  $qcValue%f * exp($beta x (2 / ($N%d x $qcValue%f) x $sciValue%f - 1 / ($N%d^2 x $qcValue%f^2) x ${sCluster.value(0, j.toInt)}%f))    (cluster $j%d)")

      x
    })

    qciUpdated.normalizeByRow
  }

  def similarityMatrix: BlockMatrix = {
    val seriesPairs = Array.fill(data.length - 1)(1).scanLeft(0)(_ + _).flatMap(i => {
      val pair = Array.fill(data.length - i - 1)(i + 1).scanLeft(0)(_ + _)
      pair.map(j => Array(ArrayIndex(i), ArrayIndex(j)))
    }).zipWithIndex.map(x => SeriesIndexCombination(x._1, x._2))

    // this is the slow step
    val offDiagonalEntries = data.makeSeriesPairs(seriesPairs).flatMap(dataPair => {
      val MI = MutualInformation(dataPair(0).series.toImmutableArray, dataPair(1).series.toImmutableArray).MI(absoluteTolerance = 0.0005)
      Seq(MatrixEntry(dataPair(0).index.toLong, dataPair(1).index.toLong, MI),
          MatrixEntry(dataPair(1).index.toLong, dataPair(0).index.toLong, MI))
    })

    val diagonalEntries = data.map(indexedSeries => {
      val length = indexedSeries.series.toImmutableArray.length
      val MI = MutualInformation.MIMax(length)
      MatrixEntry(indexedSeries.index.toLong, indexedSeries.index.toLong, MI)
    })

    new CoordinateMatrix(offDiagonalEntries ++ diagonalEntries, data.length, data.length).toBlockMatrix(blockSizeN, blockSizeN).cache()
  }

}