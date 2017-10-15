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
case class HelmholtzClustering(data: MultiSeries, T: Double, blockSizeN: Int, blockSizeNc: Int)(implicit sc: SparkContext) {


  def singlePass(S: BlockMatrix, Qci: BlockMatrix): BlockMatrix = {
    val Qc = sc.broadcast(Qci.colSums.toLocalMatrix)  // 1 x Nc
    val Sc = sc.broadcast(Qci.transpose.multiply(S).diagMultiply(Qci).toLocalMatrix)  // 1 x Nc
    val Sci = S.multiply(Qci)

    val N: Int = data.rows
    val beta: Double = 1d / T

    val QciUpdated = Sci.mapWithIndex((_, j, Sci) => {
      val qc = Qc.value(0, j.toInt)

      qc * exp(beta * (Sci * 2d / N / qc - Sc.value(0, j.toInt) / (N * N * qc * qc)))
    })

    QciUpdated.normalizeByRow
  }

  def similarityMatrix: BlockMatrix = {
    val seriesPairs = (0 until data.rows).map(ArrayIndex(_)).combinations(2).toList.map(_.toVector)
      .zipWithIndex.map(x => SeriesIndexCombination(x._1, x._2))

    // this is the slow step
    val offDiagonalEntries = data.makeSeriesPairs(seriesPairs).flatMap(dataPair => {
      val MI = MutualInformation(dataPair(0).series.toImmutableArray, dataPair(1).series.toImmutableArray).MI()
      Seq(MatrixEntry(dataPair(0).index.toLong, dataPair(1).index.toLong, MI),
          MatrixEntry(dataPair(1).index.toLong, dataPair(0).index.toLong, MI))
    })

    val diagonalEntries = data.map(indexedSeries => {
      val length = indexedSeries.series.toImmutableArray.length
      val MI = MutualInformation.MIMax(length)
      MatrixEntry(indexedSeries.index.toLong, indexedSeries.index.toLong, MI)
    })

    new CoordinateMatrix(offDiagonalEntries ++ diagonalEntries, data.rows, data.rows).toBlockMatrix(blockSizeN, blockSizeN).cache()
  }

}