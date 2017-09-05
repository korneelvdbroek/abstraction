package com.fbot.main

import breeze.linalg.max
import breeze.numerics.{digamma, log}
import com.fbot.algos.mutualinformation.MIData
import com.fbot.common.data.BigData
import com.fbot.common.fastcollections.index.ArrayIndex
import org.apache.spark.rdd.RDD
import org.apache.spark.sql.SparkSession

/**
  * TODO:
  * - make this into a case class/trait (add to MIData and rename to MutualInformation)
  * - implement the clustering algo
  *
  */
object TestMI {

  def main(args: Array[String]): Unit = {
    val spark = SparkSession.builder.appName("Simple Application").getOrCreate()
    implicit val sc = spark.sparkContext


    // Data source
    val k: Int = 10
    // higher k is lower statistical error, but higher systematic error
    val rho: Double = 0.89d
    val N: Int = 10000
    val dim: Int = 1
    val data: BigData = ConstGaussianData2d(N, 7d).data // RndDataXd(dim, N).data // FxDataXd(dim, N).data // GaussianData2d(N, rho).data

    // Sample data
    val sampleData: MIData = MIData(data(0), data(1))
    println(s"Sample size (N) = $N")

    val nxy: RDD[(Int, Int)] = sc.parallelize(1 until N).map(ii => {
      val i = ArrayIndex(ii)
      val (kNearestIndices, t1) = Utils.timeIt {
        sampleData.kNearest(sampleData.space)(k, i)
      }
      val epsilonX = kNearestIndices.map(kNearestIndex => sampleData.spaceX.distance(sampleData.points(i), sampleData.points(kNearestIndex))).max
      val epsilonY = kNearestIndices.map(kNearestIndex => sampleData.spaceY.distance(sampleData.points(i), sampleData.points(kNearestIndex))).max

      val (x, t2) = Utils.timeIt {
        (sampleData.numberOfCloseByPoints(sampleData.spaceX)(epsilonX, i),
          sampleData.numberOfCloseByPoints(sampleData.spaceY)(epsilonY, i))
      }
      if (ii % 1000 == 0) println(f"$i%12s:  ${Utils.prettyPrintTime(t1) } // ${Utils.prettyPrintTime(t2) }: $x")
      x
    })

    val ave = nxy.map(nxyi => digamma(nxyi._1) + digamma(nxyi._2)).sum / N
    val MI = max(digamma(k) - 1d / k - ave + digamma(N), 0d)

    println(s"${digamma(k) } - ${1d / k } - $ave + ${digamma(N) } = ")
    println(f" = $MI")
    println(f"ideal Gaussian = ${-1d / 2d * log(1 - rho * rho) }")
    spark.stop()
  }

}
