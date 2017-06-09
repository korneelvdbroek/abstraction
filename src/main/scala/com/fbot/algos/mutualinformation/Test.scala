package com.fbot.algos.mutualinformation

import com.fbot.common.immutable.{ArrayIndex, ImmutableArray}

import scala.util.Random

/**
  * Copyright (C) 5/29/2017 - REstore NV
  *
  */
object Test extends App {
  import Utils._


  val data = ImmutableArray(Array.fill[Tuple](1000000){
    def randomInt = {
      val sign = if (Random.nextBoolean()) 1 else -1
      sign * Random.nextInt(1000)
    }
    Tuple(randomInt, randomInt, randomInt, randomInt, randomInt, randomInt, randomInt, randomInt)
  })

  // 0 - 1,000   of 1,000,000 = 10^6
  // volume total space = 2000^8 = 2^8 10^(3*8)   = 256   10^24  --> 10^6  points
  // volume unit cube   =  500^8 = 1/2^8 10^(3*8) = 1/256 10^24  --> 15.25 points
  val k = 20
  val unitSize = 1000d
  val space = Space(Array(unitSize, unitSize, unitSize, unitSize, unitSize, unitSize, unitSize, unitSize))

  val cloud = PointCloud(data, space)

  def checkIfEqual(centerTuple: Tuple, resultBF: ImmutableArray[(ArrayIndex, Double)], result: ImmutableArray[ArrayIndex]): Boolean = {
    val resultWithDistance = result.map(index => (index, TupleOps.distance(data(index), centerTuple)))

    def cleanup(data: ImmutableArray[(ArrayIndex, Double)]): Map[Double, Set[ArrayIndex]] = {
      val maxDistance = data.map(_._2).repr.max   // filter out biggest distance since we might have multiplicities...
      data.groupBy(_._2).map(x => (x._1, x._2.map(_._1).toSet)).filterKeys(distance => distance != maxDistance)
    }

    cleanup(resultBF) == cleanup(resultWithDistance)
  }

  for (loop <- 0 to 20){
    val centerTupleIndex = ArrayIndex(Random.nextInt(data.length))
    val centerTuple = data(centerTupleIndex)

    val (resultBruteForce, tBruteForce) = timeIt { cloud.kNearestBruteForce(data.indexRange.filterNot(_ == centerTupleIndex))(k, centerTuple) }
    val (result, t) = timeIt { cloud.kNearest(k, centerTupleIndex) }

    if (!checkIfEqual(data(centerTupleIndex), resultBruteForce, result)) {
      println(resultBruteForce)
      println(result)
      throw new RuntimeException("data from algo does not match brute force")
    }

    println(f"$loop%3d: Brute Force t = ${prettyPrintTime(tBruteForce)}; t = ${prettyPrintTime(t)}")

  }

  val centerTupleIndex = ArrayIndex(Random.nextInt(data.length))
  val centerTuple = data(centerTupleIndex)


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
//  val space = Space(Array(6.0, 6.0))
//  val data = ImmutableArray(
//    Tuple(0,0),
//    Tuple(1,0), Tuple(1,1), Tuple(-1, 2), Tuple(-2,-3),
//    Tuple(5,1), Tuple(1,5), Tuple(-5,1), Tuple(1,-5),
//    Tuple(5,5), Tuple(-5,5), Tuple(-5,-5), Tuple(5,-5),
//    Tuple(6,5), Tuple(7,6))
//
//  val k = 3
//  val centerTupleIndex = ArrayIndex(9)
//
//  val cloud = PointCloud(data, space)

  val x = cloud.kNearestBruteForce(data.indexRange)(k+1, centerTuple)
  val y = cloud.kNearest(k, centerTupleIndex)
  println(x.map(index => (data(index._1), index._2)))
  println()
  println("fast algo")
  println(y.map(index => data(index)))

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