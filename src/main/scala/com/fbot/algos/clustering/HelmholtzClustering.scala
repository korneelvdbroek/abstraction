package com.fbot.algos.clustering

import breeze.numerics.{exp, log}
import com.fbot.algos.mutualinformation.MutualInformation
import com.fbot.common.data.MultiSeries
import com.fbot.common.linalg.distributed.RichBlockMatrix._
import org.apache.spark.SparkContext
import org.apache.spark.mllib.linalg.distributed.{BlockMatrix, CoordinateMatrix, MatrixEntry}

/**
  *
  */
case class MISimilarity(data: MultiSeries, blockSizeN: Int, blockSizeNc: Int)(implicit sparkContext: SparkContext) {

  def similarityMatrix: BlockMatrix = {

    val upperDiagonalElements = data.cartesian(data).flatMap(seriesPair => {
      val (series1, series2) = seriesPair

      if (series1.index < series2.index) {
        val MIMax = MutualInformation.MIMax(series1.length)
        // this is the slow step
        val MI = MutualInformation(series1.data, series2.data).MI(k = 200) //, absoluteTolerance = MIMax * 0.05)

        Seq(MatrixEntry(series1.index, series2.index, MI),
            MatrixEntry(series2.index, series1.index, MI))
      } else if (series1.index == series2.index) {
        val MIMax = MutualInformation.MIMax(series1.length)

        Seq(MatrixEntry(series1.index, series1.index, MIMax))
      } else {

        Seq.empty[MatrixEntry]
      }
    })

    new CoordinateMatrix(upperDiagonalElements, data.length, data.length).toBlockMatrix(blockSizeN, blockSizeN).cache()
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