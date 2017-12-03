package com.fbot.algos.clustering

import breeze.numerics.{exp, log}
import com.fbot.algos.mutualinformation.MutualInformation
import com.fbot.common.data.MultiSeries
import com.fbot.common.data.MultiSeries.SeriesIndexCombination
import com.fbot.common.fastcollections.ImmutableArray
import com.fbot.common.fastcollections.ImmutableArray._
import com.fbot.common.fastcollections.index.ArrayIndex
import com.fbot.common.linalg.distributed.RichBlockMatrix._
import org.apache.spark.SparkContext
import org.apache.spark.mllib.linalg.distributed.{BlockMatrix, CoordinateMatrix, MatrixEntry}

/**
  *
  */
case class MISimilarity(data: MultiSeries, blockSizeN: Int, blockSizeNc: Int)(implicit sparkContext: SparkContext) {

  def similarityMatrix: BlockMatrix = {
    val seriesPairs = ImmutableArray.range(0, data.length - 1).flatMap(i => {
      val pair = ImmutableArray.range(i + 1, data.length)
      pair.map(j => Array(ArrayIndex(i), ArrayIndex(j)))
    }).mapWithIndex((pair, index) => SeriesIndexCombination(pair, index.toInt))

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

case class HelmholtzClustering(s: BlockMatrix, temp: Double, blockSizeN: Int, blockSizeNc: Int)(implicit sparkContext: SparkContext) {

  def singlePass(qci: BlockMatrix): BlockMatrix = {
    val n: Long = s.numCols
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
      //      println(f"$x%f  =  $qcValue%f * exp($beta x (2 / ($N%d x $qcValue%f) x $sciValue%f - 1 / ($N%d^2 x $qcValue%f^2) x ${sCluster.value(0, j.toInt)
      // }%f))    (cluster $j%d)")
      x
    })

    qciUpdated.normalizeByRow
  }


  def freeEnergy(qci: BlockMatrix): Double = {
    // 1 x Nc:
    val nqc = qci.colSums

    aveS(qci, nqc) - temp * mici(qci, nqc)
  }


  /**
    * Similarity between points in cluster, averaged over all clusters
    */
  def aveS(qci: BlockMatrix): Double = {
    aveS(qci, qci.colSums)
  }

  private def aveS(qci: BlockMatrix, nqc: BlockMatrix): Double = {
    val n: Long = s.numCols

    // N x Nc: qic = qci ./ nqc
    val qic = qci.normalizeByCol
    // N x Nc:
    val sic = s.multiply(qic)
    // 1 x Nc:
    val sc = qic.transpose.diagMultiply(sic)

    nqc.transpose.multiply(sc)(0L, 0L) / n
  }

  /**
    * Mutual information between belonging to a cluster and being a particular point.
    */
  def mici(qci: BlockMatrix): Double = {
    mici(qci, qci.colSums)
  }

  private def mici(qci: BlockMatrix, nqc: BlockMatrix): Double = {
    val n: Long = s.numCols

    val clusterEntropy = -nqc.map(nqcValue => nqcValue * log(nqcValue / n)).fold(0d)(_ + _) / n

    val clusterEntropyConditionalOnI = -qci.map(qciValue => qciValue * log(qciValue)).fold(0d)(_ + _) / n

    clusterEntropy - clusterEntropyConditionalOnI
  }


}