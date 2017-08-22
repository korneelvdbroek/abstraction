package com.fbot.main

import breeze.numerics.{digamma, log}
import com.fbot.algos.mutualinformation.MIData
import com.fbot.common.fastcollections.ImmutableArray


/**
  * TODO:
  * - make this into a case class/trait
  * - implement the clustering algo
  *
  */
object TestMI extends App {
  val k: Int = 10   // higher k is lower statistical error, but higher systematic error
  val r: Double = 0.5d
  val N: Int = 40000
  val dim: Int = 1
  val sampleData: MIData = RndDataXd(dim, N).data // FxDataXd(dim, N).data // GaussianData2d(N, r).data
  println(s"Sample size (N) = $N")

  val nxy: ImmutableArray[(Int, Int)] = ImmutableArray.indexRange(0, N).map(i => {
    val (kNearestIndices, t1) = Utils.timeIt{
      sampleData.kNearest(sampleData.space)(k, i)
    }
    val epsilonX = kNearestIndices.map(kNearestIndex => sampleData.spaceX.distance(sampleData.points(i), sampleData.points(kNearestIndex))).max
    val epsilonY = kNearestIndices.map(kNearestIndex => sampleData.spaceY.distance(sampleData.points(i), sampleData.points(kNearestIndex))).max

    val (x, t2) = Utils.timeIt{
      (sampleData.numberOfCloseByPoints(sampleData.spaceX)(epsilonX, i),
       sampleData.numberOfCloseByPoints(sampleData.spaceY)(epsilonY, i))
    }
    if (i.i % 1000 == 0) println(f"$i%12s:  ${ Utils.prettyPrintTime(t1) } // ${ Utils.prettyPrintTime(t2) }: $x")
    x
  })

  println(s"${digamma(k)} - ${1d / k} - ${nxy.map(nxyi => digamma(nxyi._1) + digamma(nxyi._2)).sum / N} + ${digamma(N)} = ")
  println(f" = ${digamma(k) - 1d / k - nxy.map(nxyi => digamma(nxyi._1) + digamma(nxyi._2)).sum / N + digamma(N)}")
  println(f"ideal Gaussian = ${-1d / 2d * log(1 - r*r)}")
}
