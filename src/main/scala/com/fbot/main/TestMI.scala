package com.fbot.main

import breeze.linalg.max
import breeze.numerics.{digamma, log}
import com.fbot.algos.mutualinformation.MIData
import com.fbot.common.data.{BigData, Row}
import com.fbot.common.fastcollections.index.ArrayIndex
import org.apache.spark.HashPartitioner
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
    val N: Int = 100000
    val dim: Int = 1
    val data: BigData = GaussianData2d(N, rho).data // RndDataXd(dim, N).data // FxDataXd(dim, N).data // GaussianData2d(N, rho).data

    println(sc.defaultParallelism)

    val seriesPairs = (0 until data.rows).combinations(2).toList
      .zipWithIndex
      .map(pairIndex => ((pairIndex._1(0), pairIndex._1(1)), pairIndex._2))
      .groupBy(_._1._1).map(x => (ArrayIndex(x._1), x._2.map(y => (ArrayIndex(y._1._2), y._2))))

    // For every action performed on a dataframe, all transformations (=lazy) will be recomputed.
    // Some transformations (zipWithIndex) trigger a job!
    val parallelSeriesPairs = data
      .flatMap(row => seriesPairs.getOrElse(row.index, Nil).map(pairIndex => (pairIndex._1, (row, pairIndex._2))))
      .join(data.series).map(x => (x._2._1._2, (x._2._1._1, Row(x._1, x._2._2))))
      .partitionBy(new HashPartitioner(sc.defaultParallelism))

    parallelSeriesPairs.map(dataPair => {

      // Sample data
      val sampleData: MIData = MIData(dataPair._2._1.rowData, dataPair._2._2.rowData)
      println(s"Sample size (N) = $N")

      val nxy = (0 until sampleData.length).map(ii => {
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

      //println(s"${digamma(k) } - ${1d / k } - $ave + ${digamma(N) } = ")
      println(f"$MI%7.4f vs ${-1d / 2d * log(1 - rho * rho) }%7.4f")

      MI
    }).sum

    spark.stop()
  }


  def printPartition[T](partition: RDD[(ArrayIndex, (Row, Row))]): Unit = {
    partition.foreachPartition(it => {
      val contentStr = it.foldLeft("")((str, i) => str ++ s"${i._1}(${i._2._1.index}, ${i._2._2.index}), ")
      println(s"partition: $contentStr")
    })
  }

}
