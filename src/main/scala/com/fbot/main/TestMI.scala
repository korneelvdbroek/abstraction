package com.fbot.main

import breeze.linalg.{DenseMatrix, DenseVector, det}
import breeze.numerics.log
import com.fbot.algos.mutualinformation.MutualInformation
import com.fbot.common.data.{BigData, Row}
import com.fbot.common.fastcollections.index.ArrayIndex
import org.apache.spark.mllib.linalg.distributed.{CoordinateMatrix, MatrixEntry}
import org.apache.spark.rdd.RDD
import org.apache.spark.sql.SparkSession
import org.apache.spark.{HashPartitioner, SparkContext}
import grizzled.slf4j.Logging

/**
  * TODO:
  * - improve boxing for high dimensions (minus and cartesian are headaches) -- currently we shortcut to the bruteforce which is way faster!
  * - find formula accuracy for d = 1000  (use realistic sigma)
  * - implement the clustering algo
  *
  * References:
  * + Information based clustering
  * Noam Slonim, Gurinder Singh Atwal, Gasper Tkacik, and William Bialek
  * https://arxiv.org/pdf/q-bio/0511043.pdf
  * https://arxiv.org/pdf/q-bio/0511042.pdf
  */
object TestMI extends Logging {

  implicit class RichRows(val pairedRows: RDD[(Vector[Row], (Vector[ArrayIndex], Int))]) extends AnyVal {

    def joinOnRow(rowIndex: Int, data: BigData): RichRows = {
      pairedRows.map(x => (x._2._1(rowIndex), x)).join(data.series).map(x => (x._2._1._1 :+ Row(x._1, x._2._2), x._2._1._2))
    }

    def partitionBy()(implicit sc: SparkContext): RDD[(Int, Vector[Row])] = {
      pairedRows.map(x => (x._2._2, x._1)).partitionBy(new HashPartitioner(sc.defaultParallelism)).cache()
    }

  }

  def main(args: Array[String]): Unit = {
    val spark = SparkSession.builder.appName("Simple Application").getOrCreate()
    implicit val sc = spark.sparkContext


    // Data source
    val k: Int = 1
    // higher k is lower statistical error, but higher systematic error

    val rho: Double = -0.5d
    val N: Int = 10000
    val dim: Int = 1000

    val mu = DenseVector(Array.fill(2 * dim)(0d))
    val sigma = {
      val sigmaX = DenseMatrix.eye[Double](dim)
      val sigmaY = DenseMatrix.eye[Double](dim)
      //+ DenseMatrix((0.0, 0.0, 0.0), (0.0, 0.0, 0.0), (0.0, 0.0, 0.0))
      val sigmaXY = DenseMatrix.eye[Double](dim) * rho // ((0.1, 0.0, 0.0), (0.0, 0.1, 0.0), (0.0, 0.0, 0.1))

      val a = DenseMatrix.zeros[Double](2 * dim, 2 * dim)
      a(0 until dim, 0 until dim) := sigmaX
      a(dim until 2 * dim, dim until 2 * dim) := sigmaY
      a(0 until dim, dim until 2 * dim) := sigmaXY
      a(dim until 2 * dim, 0 until dim) := sigmaXY.t

      a
    }
    info(s"sigma = $sigma")
    val data: BigData = GaussianData(2, N, dim, sigma, mu)
      .data // GaussianData.data // RndDataXd(dim, N).data // FxDataXd(dim, N).data // GaussianData2d(N, rho).data

    info(sc.defaultParallelism)

    val seriesPairs = (0 until data.rows).map(ArrayIndex(_)).combinations(2).toList.map(_.toVector)
      .zipWithIndex


    // For every action performed on a dataframe, all transformations (=lazy) will be recomputed.
    // Some transformations (zipWithIndex) trigger a job!
    val parallelSeriesPairs = data
      .flatMap(row => seriesPairs.groupBy(_._1(0)).getOrElse(row.index, Nil).map(pairIndex => (Vector(row), pairIndex)))
      .joinOnRow(1, data)
      .partitionBy()


    //    printPartition(parallelSeriesPairs)

    val similarityMatrix = new CoordinateMatrix(parallelSeriesPairs.map(dataPair => {

      // Sample data
      val sampleData = MutualInformation(dataPair._2(0).rowData, dataPair._2(1).rowData)
      info(s"Sample size (N) = ${sampleData.length }")

      val MI = sampleData.MI(k)

      val MIGaussian1 = -1d / 2d * log(det(sigma))
      val MIGaussian2 = 1d / 2d * log(det(sigma(0 until dim, 0 until dim)))
      val MIGaussian3 = 1d / 2d * log(det(sigma(dim until 2 * dim, dim until 2 * dim)))

      info(f"$MI%7.4f vs ${MIGaussian1 + MIGaussian2 + MIGaussian3 }%7.4f = $MIGaussian1%7.4f + $MIGaussian2%7.4f + $MIGaussian3%7.4f  " +
              f"${100.0 * (MI - (MIGaussian1 + MIGaussian2 + MIGaussian3)) / (MIGaussian1 + MIGaussian2 + MIGaussian3) }%7.2f%%")

      MatrixEntry(dataPair._2(0).index.toLong, dataPair._2(1).index.toLong, MI)
    }).cache(), data.rows, data.rows)

    info(similarityMatrix.toBlockMatrix.toLocalMatrix)

    spark.stop()
  }


  def printPartition[T](partition: RDD[(Int, Vector[Row])]): Unit = {
    partition.foreachPartition(it => {
      val contentStr = it.foldLeft("")((str, i) => str ++ s"${i._1 }(${i._2.map(_.index) }), ")
      info(s"partition: $contentStr")
    })
  }

}
