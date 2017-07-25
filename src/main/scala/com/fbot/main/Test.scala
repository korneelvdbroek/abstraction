package com.fbot.main

import com.fbot.algos.mutualinformation.MIData
import com.fbot.common.fastcollections.ImmutableArray
import com.fbot.common.fastcollections.index.ArrayIndex
import com.fbot.common.hyperspace._

import scala.util.Random

/**
  *
  */
object Test extends App {
  import Utils._

  val k = 4
  val cloud = data8dim

  for (loop <- 0 to 20){
    val centerTupleIndex = ArrayIndex(Random.nextInt(cloud.points.length))
    val centerTuple = cloud.points(centerTupleIndex)

    val allPoints = cloud.points.indexRange.filterNot(_ == centerTupleIndex)
    val (resultBF, tBruteForce) = timeIt { cloud.kNearestBruteForce(cloud.spaceX, allPoints)(k, centerTuple) }
    val resultBruteForce = resultBF.map(_._1)
    val (result, t) = timeIt { cloud.kNearest(cloud.spaceX)(k, centerTupleIndex) }

//    val allPoints = cloud.points.indexRange
//    val distance = 200d
//    val (resultBruteForce, tBruteForce) = timeIt { cloud.numberOfCloseByPointsBruteForce(space, allPoints)(distance, centerTuple) }
//    val (result, t) = timeIt { cloud.numberOfCloseByPoints(space)(distance, centerTupleIndex) }
//    println(s"$resultBruteForce -- $result")

    checkIfEqual(cloud.spaceX)(cloud.points(centerTupleIndex), resultBruteForce, result)

    println(f"$loop%3d: Brute Force t = ${prettyPrintTime(tBruteForce)}; t = ${prettyPrintTime(t)}")

  }

  val centerTupleIndex = ArrayIndex(Random.nextInt(cloud.points.length))
  val centerTuple = cloud.points(centerTupleIndex)




  val x = cloud.kNearestBruteForce(cloud.spaceX, cloud.points.indexRange.filterNot(_ == centerTupleIndex))(k+1, centerTuple)
  val y = cloud.kNearest(cloud.spaceX)(k, centerTupleIndex)
  println(x.map(index => (cloud.points(index._1), index._2)))
  println()
  println("fast algo")
  println(y.map(index => cloud.points(index)))



  private def data8dim: MIData = {
    def randomInt = {
      val sign = if (Random.nextBoolean()) 1 else -1
      sign * Random.nextInt(1000)
    }

    val dataX = ImmutableArray(Array.fill[Tuple](1000000)(Tuple(randomInt, randomInt, randomInt, randomInt)))
    val dataY = ImmutableArray(Array.fill[Tuple](1000000)(Tuple(randomInt, randomInt, randomInt, randomInt)))

    // 0 - 1,000   of 1,000,000 = 10^6
    // volume total space = 2000^8 = 2^8 10^(3*8)   = 256   10^24  --> 10^6  points
    // volume unit cube   =  500^8 = 1/2^8 10^(3*8) = 1/256 10^24  --> 15.25 points
    MIData(dataX, dataY)
  }

  private def data2dim: MIData = {
    //  5432101234567
    //
    //              4 6
    //  0     6   93  5
    //                4
    //                3
    //      3         2
    //  7     2   5   1
    //       01       0
    //                1
    //                2
    //     4          3
    //                4
    //  1     8   2   5
    //
    val space = Space(ImmutableArray.indexRange(0,2), ImmutableArray(6.0, 6.0))
    val data = ImmutableArray(
      Tuple(0,0),
      Tuple(1,0), Tuple(1,1), Tuple(-1, 2), Tuple(-2,-3),
      Tuple(5,1), Tuple(1,5), Tuple(-5,1), Tuple(1,-5),
      Tuple(5,5), Tuple(-5,5), Tuple(-5,-5), Tuple(5,-5),
      Tuple(6,5), Tuple(7,6))

    val centerTupleIndex = ArrayIndex(9)

    MIData(data, data)
  }


  private def checkIfEqual(space: HyperSpace)(centerTuple: Tuple, resultBF: ImmutableArray[ArrayIndex], result: ImmutableArray[ArrayIndex]): Unit = {
    val resultWithDistance = result.map(index => (index, space.distance(cloud.points(index), centerTuple)))
    val resultBFWithDistance = resultBF.map(index => (index, space.distance(cloud.points(index), centerTuple)))

    def removeLargestDistance(data: ImmutableArray[(ArrayIndex, Double)]): Map[Double, Set[ArrayIndex]] = {
      val maxDistance = data.map(_._2).repr.max   // filter out biggest distance since we might have multiplicities...
      data.groupBy(_._2).map(x => (x._1, x._2.map(_._1).toSet)).filterKeys(distance => distance != maxDistance)
    }

    if (removeLargestDistance(resultBFWithDistance) != removeLargestDistance(resultWithDistance)) {
      println(removeLargestDistance(resultBFWithDistance))
      println(removeLargestDistance(resultWithDistance))
      throw new RuntimeException("data from algo does not match brute force")
    }
  }

}

object Utils {

  def timeIt[R](block: => R): (R, Long) = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    (result, t1 - t0)
  }

  def prettyPrintTime(i: Long): String = {
    val ms = i / 1000000
    val us = (i / 1000) % 1000
    val ns = i % 1000
    f"$ms%6d ms $us%3d us $ns%3d ns"
  }

}