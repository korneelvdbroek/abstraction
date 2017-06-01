package com.fbot.algos.mutualinformation

import scala.util.Random

/**
  * Copyright (C) 5/29/2017 - REstore NV
  *
  */
object Test extends App {


  val cloud = TupleCloud(Array.fill[Tuple](1000000){
    def randomInt = {
      val sign = if (Random.nextBoolean()) 1 else -1
      sign * Random.nextInt(1000)
    }
    Tuple(randomInt, randomInt, randomInt, randomInt, randomInt, randomInt, randomInt, randomInt)
  })
  val kTrial = 4
  val centerTupleIndex = TupleIndex(Random.nextInt(cloud.tupleCloud.length))

  //  val cloud: TupleCloud = TupleCloud(
//    Tuple(0,0), Tuple(1,0), Tuple(1,1), Tuple(-1, 2), Tuple(-2,-3),
//    Tuple(5,1), Tuple(1,5), Tuple(-5,1), Tuple(1,-5),
//    Tuple(5,5), Tuple(-5,5), Tuple(-5,-5), Tuple(5,-5),
//    Tuple(6,5), Tuple(7,6))
//
//  val kTrial = 1
//  val centerTupleIndex = TupleIndex(0)

  val (k, _) = cloud.nearestBruteForce(kTrial, centerTupleIndex)


  println(s"start...")
  for (i <- 1 to 20) {
    val tBF = timeIt { cloud.nearestBruteForce(k, centerTupleIndex) }
    val t   = timeIt { cloud.nearest(k, centerTupleIndex) }

    println(f"${prettyPrint(tBF)} vs ${prettyPrint(t)} (${100.0*t.toDouble/tBF}%3.2f %%)")
  }

  val (_, kNearestBFIndices) = cloud.nearestBruteForce(k, centerTupleIndex)
  val kNearestIndices = cloud.nearest(k, centerTupleIndex)

  if (kNearestIndices.toSet != kNearestBFIndices.toSet) println("ERROR")

  val kNearest   = TupleArray(kNearestIndices.map(cloud.tupleCloud(_)))
  val kNearestBF = TupleArray(kNearestBFIndices.map(cloud.tupleCloud(_)))

//  println(s"All tuples:\n${cloud.tupleCloud}")
//  println(s"Center tuple:\n${ cloud.tupleCloud(centerTupleIndex) }")
  println(s"$k-Nearest              :\n$kNearest")
  println(s"$k-Nearest (brute force):\n$kNearestBF")



  def timeIt[R](block: => R): Long = {
    val t0 = System.nanoTime()
    block    // call-by-name
    val t1 = System.nanoTime()
    t1 - t0
  }

  def prettyPrint(i: Long): String = {
    val ms = i / 1000000
    val us = (i / 1000) % 1000
    val ns = i % 1000
    f"$ms%6d s $us%3d us $ns%3d ns"
  }
}
