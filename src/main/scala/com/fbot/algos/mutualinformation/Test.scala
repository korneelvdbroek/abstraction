package com.fbot.algos.mutualinformation

import scala.util.Random

/**
  * Copyright (C) 5/29/2017 - REstore NV
  *
  */
object Test extends App {

//  val cloud = PointCloud(Array.fill[Tuple](1000000){
//    def randomInt = {
//      val sign = if (Random.nextBoolean()) 1 else -1
//      sign * Random.nextInt(1000)
//    }
//    Tuple(randomInt, randomInt, randomInt, randomInt, randomInt, randomInt, randomInt, randomInt)
//  })
//  val kTrial = 4
//  val centerTupleIndex = ArrayIndex(Random.nextInt(cloud.points.length))


//  val cloud: TupleCloud = TupleCloud(
//    Tuple(0,0), Tuple(1,0), Tuple(1,1), Tuple(-1, 2), Tuple(-2,-3),
//    Tuple(5,1), Tuple(1,5), Tuple(-5,1), Tuple(1,-5),
//    Tuple(5,5), Tuple(-5,5), Tuple(-5,-5), Tuple(5,-5),
//    Tuple(6,5), Tuple(7,6))
//
//  val kTrial = 1
//  val centerTupleIndex = TupleIndex(0)


  val space = Space(3, Array(1.0, 1.0, 1.0))

  val bigCube   = HyperCube(UnitHyperCube(0.0, 1.0, 1.0), UnitHyperCube(4.0, 3.0, 3.0))
  val smallCube = HyperCube(UnitHyperCube(1.0, 1.0, 1.0), UnitHyperCube(3.0, 3.0, 3.0))

  val unitCube = UnitHyperCube(0.0, 1.0, 1.0)

  println(unitCube.isIn(smallCube))
  println(unitCube.isIn(bigCube))
}

object Utils {

  def timeIt[R](block: => R): Long = {
    val t0 = System.nanoTime()
    block    // call-by-name
    val t1 = System.nanoTime()
    t1 - t0
  }

  def prettyPrintTime(i: Long): String = {
    val ms = i / 1000000
    val us = (i / 1000) % 1000
    val ns = i % 1000
    f"$ms%6d s $us%3d us $ns%3d ns"
  }

}