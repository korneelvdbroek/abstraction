package com.fbot.main

import breeze.numerics.{digamma, log}
import com.fbot.algos.mutualinformation.MIData
import com.fbot.common.fastcollections.ImmutableArray


/**
  * Copyright (C) 8/16/2017 - REstore NV
  *
  */
object TestMI extends App {
  val k: Int = 10
  val r: Double = 0.9d
  val sampleData: MIData = GaussianData2d(4000, r).data
  val N = sampleData.length
  println(s"Sample size (N) = $N")

  val nxy: ImmutableArray[(Int, Int)] = ImmutableArray.indexRange(0, N).map(i => {
    val (iNear, t1) = Utils.timeIt{
      val kNearest = sampleData.kNearest(sampleData.space)(k, i)
      kNearest.last
    }
    val epsilonX = sampleData.spaceX.distance(sampleData.points(i), sampleData.points(iNear))
    val epsilonY = sampleData.spaceY.distance(sampleData.points(i), sampleData.points(iNear))

    val (x, t2) = Utils.timeIt{
      (sampleData.numberOfCloseByPoints(sampleData.spaceX)(epsilonX, i),
       sampleData.numberOfCloseByPoints(sampleData.spaceY)(epsilonY, i))
    }
    if (i.i % 1000 == 0) println(f"$i%12s:  ${ Utils.prettyPrintTime(t1) } // ${ Utils.prettyPrintTime(t2) }: $x")
    x
  })

  println(s"${digamma(k)} - ${1d / k} - ${nxy.map(nxyi => digamma(nxyi._1) + digamma(nxyi._2)).sum / N} + ${digamma(N)} = ")
  println(digamma(k) - 1d / k - nxy.map(nxyi => digamma(nxyi._1) + digamma(nxyi._2)).sum / N + digamma(N))
  println(-1d / 2d * log(1 - r*r))
}
