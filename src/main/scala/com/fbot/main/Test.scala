package com.fbot.main

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
  val cloud = RndDataXd(4, 1000000).data

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