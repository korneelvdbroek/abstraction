package com.fbot.algos.mutualinformation

import com.fbot.algos.mutualinformation.TupleOps._
import com.fbot.common.immutable.LongArrayMath._
import com.fbot.common.immutable.{ArrayIndex, ImmutableArray, LongArrayMath, UnzippedMap}

import scala.annotation.tailrec

/**
  * Copyright (C) 5/30/2017 - REstore NV
  *
  *
  * note: use classes to render data type safe, do everything else functional
  */
case class PointCloud(points: ImmutableArray[Tuple], space: HyperSpace) {

  def kNearestBruteForce(pointIndices: ImmutableArray[ArrayIndex])(k: Int, currentTuple: Tuple): ImmutableArray[(ArrayIndex, Double)] = {
    println(pointIndices.length)
    val otherTuplesSortedByDistance = pointIndices
      .map(index => (index, distance(points(index), currentTuple)))
      .sortBy(_._2)

    otherTuplesSortedByDistance.take(k)
  }

  /*
   New algo:
   1. OK def tuple => hyperCubeBin & store in map
   2. OK groupBy(tuple => hyperCubeBin)
   3. around center-point initialize with cube = UnitHyperCube
   4. find all points .isIn(bigCube) and not already visited
      if (number of points in hyperCube > k) find brute force k-nearest on all points
   4. if distance from farthest of k-nearest to center-point < some edges of hyperCube (or number of points in hyperCube < k)
      extend smallHyperCube -> bigHyperCube in direction of edges which are too close and go back to 4.
   */

  lazy val binnedPoints: ImmutableArray[UnitHyperCube] = points.map(space.findEnclosingUnitHyperCube)

  lazy val pointsByBin: UnzippedMap[UnitHyperCube, ImmutableArray[ArrayIndex]] = points.indexRange.unzippedGroupBy(binnedPoints(_))

  def kNearest(k: Int, currentTupleIndex: ArrayIndex): ImmutableArray[ArrayIndex] = {

    val currentTuple = points(currentTupleIndex)

    @tailrec
    def kNearestInCube(kNearestCandidates: ImmutableArray[ArrayIndex],
                       remainingPointsByBin: UnzippedMap[UnitHyperCube, ImmutableArray[ArrayIndex]],
                       cube: HyperCube): ImmutableArray[(ArrayIndex, Double)] = {

      println(s"#remainingPointsByBin = ${remainingPointsByBin.filter.length}")

      val (newCandidateBins, time) = Utils.timeIt { remainingPointsByBin.filterKeys(_ isIn cube) }
      println(s"filter cubes: ${ Utils.prettyPrintTime(time )}")

      val newCandidatePoints = if (kNearestCandidates.isEmpty) {
        newCandidateBins.values.flatten.filterNot(_ == currentTupleIndex)
      } else {
        kNearestCandidates ++ newCandidateBins.values.flatten
      }

      //println(f"$cube%60s: #candidates = ${kNearestCandidates.length }%3d + ${newCandidateBins.values.flatten.length }%3d")

      if (newCandidatePoints.length >= k) {
        val kNearestWithDistance = kNearestBruteForce(newCandidatePoints)(k, currentTuple)
        val epsilon = kNearestWithDistance.map(_._2).last

        val growLeft = space.axes.map(axis => if ((currentTuple(axis) - cube.left.repr(axis) * space.unitCubeSize(axis)) < epsilon) -1L else 0L)
        val growRight = space.axes.map(axis => if ((cube.right.repr(axis) * space.unitCubeSize(axis) - currentTuple(axis)) < epsilon) 1L else 0L)

        if (growLeft.forall(_ == 0L) && growRight.forall(_ == 0L)) {
          println("done")
          kNearestWithDistance
        } else {
          println(s"grow cube $growLeft $growRight")
          val newCube = cube.grow(growLeft, growRight)
          kNearestInCube(kNearestWithDistance.map(_._1), remainingPointsByBin.filterOut(newCandidateBins.filter), newCube)
        }

      } else {
        val newCube = cube.grow(-LongArrayMath.one(space.dim), LongArrayMath.one(space.dim))

        kNearestInCube(newCandidatePoints, remainingPointsByBin.filterOut(newCandidateBins.filter), newCube)
      }
    }

    // initialize
    val cube = space.unitCube(points(currentTupleIndex))
    val kNearestCandidates = ImmutableArray.empty[ArrayIndex]
    kNearestInCube(kNearestCandidates, pointsByBin, cube).map(_._1)
  }


}

object PointCloud {

  def print[T](x: Array[T]): String = x.deep.mkString("Array(", ",", ")")

}

/**
  *  1.  watch kids
  *  2.  schools to call (Montesorri school Ghent, sisters of Heverlee, steinerschool leuven, nos enfants, ludgardis, montesorri school antwerp)
  */
