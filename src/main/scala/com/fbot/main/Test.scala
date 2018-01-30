package com.fbot.main

import breeze.linalg.{DenseMatrix, DenseVector}
import com.fbot.algos.mutualinformation.MutualInformation
import com.fbot.common.fastcollections.Tuple
import com.fbot.common.fastcollections._
import com.fbot.common.hyperspace._
import org.apache.spark.sql.SparkSession

import scala.util.Random

/**
  * Copyright (C) 2017-2018  korneelvdbroek
  *
  * This program is free software: you can redistribute it and/or modify
  * it under the terms of the GNU General Public License as published by
  * the Free Software Foundation, either version 3 of the License, or
  * (at your option) any later version.
  *
  * This program is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU General Public License for more details.
  *
  * You should have received a copy of the GNU General Public License
  * along with this program.  If not, see <http://www.gnu.org/licenses/>.
  *
  *
  * TODO:
  *  - even though we should not be partitioning // we still are (and have blocks with super small amount of points in them) ==> BUG IN ALGO
  *  - find heuristic for optimal number in a spaceUnit (based on uniform distribution)
  *  - make MI based on RDD
  */
object Test extends App {

  import Utils._

  val spark = SparkSession.builder.appName("Simple Application").getOrCreate()
  implicit val sc = spark.sparkContext

  val k = 10
  val dim = 3
  val N = 10000

  val mu = DenseVector(Array.fill(2 * dim)(0d))
  val sigma = {
    val sigmaX = DenseMatrix.eye[Double](dim)
    val sigmaY = DenseMatrix.eye[Double](dim)
    val sigmaXY = DenseMatrix.eye[Double](dim) * 0.1

    val a = DenseMatrix.zeros[Double](2 * dim, 2 * dim)
    a(0 until dim, 0 until dim) := sigmaX
    a(dim until 2 * dim, dim until 2 * dim) := sigmaY
    a(0 until dim, dim until 2 * dim) := sigmaXY
    a(dim until 2 * dim, 0 until dim) := sigmaXY.t

    a
  }

  val bigData = GaussianData(2, N, dim, sigma, mu).data
  // GaussianData(2, N, dim, sigma, mu).data // RndDataXd(dim, N).data
  val cloud: MutualInformation = MutualInformation(bigData(0), bigData(1))

  val space = cloud.space


  var aveTime: Long = 0L
  for (loop <- 0 until 20) {
    val centerTupleIndex = ArrayIndex(Random.nextInt(cloud.points.length))
    val centerTuple = cloud.points(centerTupleIndex)

    // Brute force
    val allPoints = cloud.points.indexRange.filterNot(_ == centerTupleIndex)
    val (resultBF, tBruteForce) = timeIt { cloud.kNearestBruteForce(space, allPoints)(k, centerTuple) }
    val resultBruteForce = resultBF._1

    // Optimized
    val (result, t) = timeIt { cloud.kNearest(space)(k, centerTupleIndex) }

    aveTime = (aveTime * loop + t) / (loop + 1)

    //    val allPoints = cloud.points.indexRange
    //    val distance = 200d
    //    val (resultBruteForce, tBruteForce) = timeIt { cloud.numberOfCloseByPointsBruteForce(space, allPoints)(distance, centerTuple) }
    //    val (result, t) = timeIt { cloud.numberOfCloseByPoints(space)(distance, centerTupleIndex) }
    //    println(s"$resultBruteForce -- $result")

    checkIfEqual(space)(cloud.points(centerTupleIndex), resultBruteForce, result)

    println(f"$loop%3d: Brute Force t = ${prettyPrintTime(tBruteForce) }; t = ${prettyPrintTime(t) }")

  }
  println(f"ave time = $aveTime (${prettyPrintTime(aveTime) })")

  val centerTupleIndex = ArrayIndex(Random.nextInt(cloud.points.length))
  val centerTuple = cloud.points(centerTupleIndex)


  val x = cloud.kNearestBruteForce(space, cloud.points.indexRange.filterNot(_ == centerTupleIndex))(k + 1, centerTuple)
  val y = cloud.kNearest(space)(k, centerTupleIndex)
  println("BF:")
  println(x)
  println()
  println("fast algo")
  println(y)


  private def checkIfEqual(space: HyperSpace)(centerTuple: Tuple, resultBF: ImmutableArray[ArrayIndex], result: ImmutableArray[ArrayIndex]): Unit = {
    val resultWithDistance = result.mapToNewType(index => (index, space.distance(cloud.points(index), centerTuple)))
    val resultBFWithDistance = resultBF.mapToNewType(index => (index, space.distance(cloud.points(index), centerTuple)))

    def removeLargestDistance(data: ImmutableArray[(ArrayIndex, Double)]): Map[Double, Set[ArrayIndex]] = {
      val maxDistance = data.mapToNewType(_._2).repr.max // filter out biggest distance since we might have multiplicities...
      data.groupBy(_._2).map(x => (x._1, x._2.mapToNewType(x => x._1).toSet)).filterKeys(distance => distance != maxDistance)
    }

    if (removeLargestDistance(resultBFWithDistance) != removeLargestDistance(resultWithDistance)) {
      val byDistanceBF = removeLargestDistance(resultBFWithDistance)
      val byDistanceBFKeys = byDistanceBF.keys.toList.sorted

      println(s"centerTuple = $centerTuple")

      println(s"BF: ")
      byDistanceBFKeys foreach (distance => {
        println(s" $distance --> Set(${byDistanceBF(distance).map(pointIndex => space.hyperSpaceUnitAround(cloud.points(pointIndex))) })")
      })

      val byDistance = removeLargestDistance(resultWithDistance)
      val byDistanceKeys = byDistance.keys.toList.sorted

      println(s"Fast: ")
      byDistanceKeys foreach (distance => {
        println(s" $distance --> Set(${byDistance(distance).map(pointIndex => space.hyperSpaceUnitAround(cloud.points(pointIndex))) })")
      })

      throw new RuntimeException("data from algo does not match brute force")
    }
  }

}
