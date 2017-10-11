package com.fbot.algos.clustering

import com.fbot.algos.mutualinformation.MutualInformation
import com.fbot.common.data.MultiSeries
import com.fbot.common.data.MultiSeries.SeriesIndexCombination
import com.fbot.common.fastcollections.index.ArrayIndex
import org.apache.spark.SparkContext
import org.apache.spark.mllib.linalg.distributed.{BlockMatrix, CoordinateMatrix, MatrixEntry}

/**
  *
  */
case class HelmholtzClustering(data: MultiSeries) {

  def similarityMatrix(implicit sc: SparkContext): BlockMatrix = {
    val seriesPairs = (0 until data.rows).map(ArrayIndex(_)).combinations(2).toList.map(_.toVector)
      .zipWithIndex.map(x => SeriesIndexCombination(x._1, x._2))

    val parallelSeriesPairs = data.makeSeriesPairs(seriesPairs).cache()

    // this is the slow step
    val offDiagonalEntries = parallelSeriesPairs.flatMap(dataPair => {
      val MI = MutualInformation(dataPair(0).series.toImmutableArray, dataPair(1).series.toImmutableArray).MI()
      Seq(MatrixEntry(dataPair(0).index.toLong, dataPair(1).index.toLong, MI),
          MatrixEntry(dataPair(1).index.toLong, dataPair(0).index.toLong, MI))
    })

    val diagonalEntries = data.map(indexedSeries => {
      val length = indexedSeries.series.toImmutableArray.length
      val MI = MutualInformation.MIMax(length)
      MatrixEntry(indexedSeries.index.toLong, indexedSeries.index.toLong, MI)
    })

    new CoordinateMatrix(offDiagonalEntries ++ diagonalEntries, data.rows, data.rows).toBlockMatrix().cache()
  }

}