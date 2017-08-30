package com.fbot.main

import org.apache.spark.sql.SparkSession
import breeze.numerics.{digamma, log}
import com.fbot.algos.mutualinformation.MIData
import com.fbot.common.fastcollections.ImmutableArray
import com.fbot.common.fastcollections.index.ArrayIndex
import org.apache.spark.rdd.RDD


/**
  * TODO:
  * - make this into a case class/trait (add to MIData)
  * - implement the clustering algo
  *
  */
object TestMI {

  def main(args: Array[String]): Unit = {
    val spark = SparkSession.builder.appName("Simple Application").getOrCreate()


    val k: Int = 10   // higher k is lower statistical error, but higher systematic error
    val r: Double = 0.89d
    val N: Int = 100000
    val dim: Int = 1
    val sampleData: MIData = GaussianData2d(N, r).data // RndDataXd(dim, N).data // FxDataXd(dim, N).data // GaussianData2d(N, r).data
    println(s"Sample size (N) = $N")

    val nxy: RDD[(Int, Int)] = spark.sparkContext.parallelize(1 until N).map(ii => {
      val i = ArrayIndex(ii)
      val (kNearestIndices, t1) = Utils.timeIt{
        sampleData.kNearest(sampleData.space)(k, i)
      }
      val epsilonX = kNearestIndices.map(kNearestIndex => sampleData.spaceX.distance(sampleData.points(i), sampleData.points(kNearestIndex))).max
      val epsilonY = kNearestIndices.map(kNearestIndex => sampleData.spaceY.distance(sampleData.points(i), sampleData.points(kNearestIndex))).max

      val (x, t2) = Utils.timeIt{
        (sampleData.numberOfCloseByPoints(sampleData.spaceX)(epsilonX, i),
         sampleData.numberOfCloseByPoints(sampleData.spaceY)(epsilonY, i))
      }
      if (ii % 1000 == 0) println(f"$i%12s:  ${ Utils.prettyPrintTime(t1) } // ${ Utils.prettyPrintTime(t2) }: $x")
      x
    })

    val ave = nxy.map(nxyi => digamma(nxyi._1) + digamma(nxyi._2)).sum / N

    println(s"${digamma(k)} - ${1d / k} - $ave + ${digamma(N)} = ")
    println(f" = ${digamma(k) - 1d / k - ave + digamma(N)}")
    println(f"ideal Gaussian = ${-1d / 2d * log(1 - r*r)}")
    spark.stop()
  }

}
