package com.fbot.algos.clustering

import breeze.numerics.{exp, log}
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
case class HelmholtzClustering(data: MultiSeries, temp: Double, blockSizeN: Int, blockSizeNc: Int)(implicit sparkContext: SparkContext) {


  def singlePass(s: BlockMatrix, qci: BlockMatrix): BlockMatrix = {
    val n: Int = data.length
    val beta: Double = 1d / temp

    // 1 x Nc:
    val nqc = sparkContext.broadcast(qci.colSums.toLocalMatrix)

    // N x Nc: qic = qci ./ nqc
    val qic = qci.normalizeByCol
    // N x Nc:
    val sic = s.multiply(qic)
    // 1 x Nc:
    val sc = sparkContext.broadcast(qic.transpose.diagMultiply(sic).toLocalMatrix)

    val qciUpdated = sic.mapWithIndex((_, j, sciValue) => {
      val x = nqc.value(0, j.toInt) / n * exp(beta * (sciValue * 2d - sc.value(0, j.toInt)))
//      println(f"$x%f  =  $qcValue%f * exp($beta x (2 / ($N%d x $qcValue%f) x $sciValue%f - 1 / ($N%d^2 x $qcValue%f^2) x ${sCluster.value(0, j.toInt)}%f))    (cluster $j%d)")
      x
    })

    qciUpdated.normalizeByRow
  }


  def freeEnergy(s: BlockMatrix, qci: BlockMatrix): Double = {
    // 1 x Nc:
    val nqc = qci.colSums

    aveS(s, qci, nqc) - temp * mici(qci, nqc)
  }


  /**
    * Similarity between points in cluster, averaged over all clusters
    */
  def aveS(s: BlockMatrix, qci: BlockMatrix): Double = {
    aveS(s, qci, qci.colSums)
  }

  private def aveS(s: BlockMatrix, qci: BlockMatrix, nqc: BlockMatrix): Double = {
    val n: Int = data.length

    // N x Nc: qic = qci ./ nqc
    val qic = qci.normalizeByCol
    // N x Nc:
    val sic = s.multiply(qic)
    // 1 x Nc:
    val sc = qic.transpose.diagMultiply(sic)

    nqc.transpose.multiply(sc)(0L,0L) / n
  }

  /**
    * Mutual information between belonging to a cluster and being a particular point.
    */
  def mici(qci: BlockMatrix): Double = {
    mici(qci, qci.colSums)
  }

  private def mici(qci: BlockMatrix, nqc: BlockMatrix): Double = {
    val n: Int = data.length

    val clusterEntropy = - nqc.map(nqcValue => nqcValue * log(nqcValue / n)).fold(0d)(_ + _) / n

    val clusterEntropyConditionalOnI = - qci.map(qciValue => qciValue * log(qciValue)).fold(0d)(_ + _) / n

    clusterEntropy - clusterEntropyConditionalOnI
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