package com.fbot.main

import breeze.linalg.{DenseMatrix, DenseVector, det}
import breeze.numerics.log
import com.fbot.algos.mutualinformation.MutualInformation
import com.fbot.common.data.MultiSeries.SeriesIndexCombination
import com.fbot.common.data.{IndexedSeries, MultiSeries}
import grizzled.slf4j.Logging
import org.apache.spark.mllib.linalg.distributed.{CoordinateMatrix, MatrixEntry}
import org.apache.spark.rdd.RDD
import org.apache.spark.sql.SparkSession

/**
  * TODO:
  * - random sampling to get good and fast convergence --> foldLeft with early stopping criteria in ImmutableArray
  * - Series as RDD[ImmutableArray[Tuple]]
  * - implement the clustering algo
  *
  * References:
  * + Information based clustering
  * Noam Slonim, Gurinder Singh Atwal, Gasper Tkacik, and William Bialek
  * https://arxiv.org/pdf/q-bio/0511043.pdf  (main paper)
  * https://arxiv.org/pdf/q-bio/0511042.pdf  (supplementary material)
  */
object TestMI extends Logging {

  def main(args: Array[String]): Unit = {
    val spark = SparkSession.builder.appName("Simple Application").getOrCreate()
    implicit val sc = spark.sparkContext


    // Data source
    val k: Int = 10
    // higher k is lower statistical error, but higher systematic error

    val rho: Double = 0.0d
    val N: Int = 1000000
    // should be > 10^{2d}
    val dim: Int = 3


    val mu = DenseVector(Array.fill(2 * dim)(0d))
    val sigma = {
      val sigmaX = DenseMatrix.eye[Double](dim)
      val sigmaY = DenseMatrix.eye[Double](dim) + DenseMatrix((0.0, 0.2, 0.0), (0.2, 0.0, 0.0), (0.0, 0.0, 0.0))
      val sigmaXY = DenseMatrix.eye[Double](dim) * rho + DenseMatrix((0.4, 0.0, -0.3), (0.0, 0.0, 0.0), (0.5, 0.0, 0.4))

      val a = DenseMatrix.zeros[Double](2 * dim, 2 * dim)
      a(0 until dim, 0 until dim) := sigmaX
      a(dim until 2 * dim, dim until 2 * dim) := sigmaY
      a(0 until dim, dim until 2 * dim) := sigmaXY
      a(dim until 2 * dim, 0 until dim) := sigmaXY.t

      a
    }
    info(s"sigma = $sigma")
    val data: MultiSeries = GaussianData(2, N, dim, sigma, mu)
      .data // GaussianData.data // RndDataXd(dim, N).data // FxDataXd(dim, N).data // GaussianData2d(N, rho).data

    info(sc.defaultParallelism)

    val seriesPairs = (0 until data.length).combinations(2).toArray.map(_.toArray)
      .zipWithIndex.map(x => SeriesIndexCombination(x._1.map(_.toLong), x._2))


    // For every action performed on a dataframe, all transformations (=lazy) will be recomputed.
    // Some transformations (zipWithIndex) trigger a job!
    val parallelSeriesPairs = data.cartesian(data).cache()


    //    printPartition(parallelSeriesPairs)

    val similarityMatrix = new CoordinateMatrix(parallelSeriesPairs.map(dataPair => {

      // Sample data
      val sampleData = MutualInformation(dataPair._1.series.toImmutableArray, dataPair._2.series.toImmutableArray)


      val MIGaussian1 = -1d / 2d * log(det(sigma))
      val MIGaussian2 = 1d / 2d * log(det(sigma(0 until dim, 0 until dim)))
      val MIGaussian3 = 1d / 2d * log(det(sigma(dim until 2 * dim, dim until 2 * dim)))

      info(s"Sample size (N) = ${sampleData.length }")
      info(f"Gaussian MI = ${MIGaussian1 + MIGaussian2 + MIGaussian3 }%7.4f = $MIGaussian1%7.4f + $MIGaussian2%7.4f + $MIGaussian3%7.4f")
      info(f"Max      MI = ${MutualInformation.MIMax(sampleData.length, k) }%7.4f")
      val MI = sampleData.MI(k)

      info(f"$MI%7.4f " +
           f"vs ${MIGaussian1 + MIGaussian2 + MIGaussian3 }%7.4f " +
           f"(${100.0 * (MI - (MIGaussian1 + MIGaussian2 + MIGaussian3)) / (MIGaussian1 + MIGaussian2 + MIGaussian3) }%7.2f%%) ")

      MatrixEntry(dataPair._1.index.toLong, dataPair._2.index.toLong, MI)
    }).cache(), data.length, data.length)

    info(similarityMatrix.toBlockMatrix.toLocalMatrix)

    spark.stop()
  }


  def printPartition[T](partition: RDD[(Int, Vector[IndexedSeries])]): Unit = {
    partition.foreachPartition(it => {
      val contentStr = it.foldLeft("")((str, i) => str ++ s"${i._1 }(${i._2.map(_.index) }), ")
      info(s"partition: $contentStr")
    })
  }

}
