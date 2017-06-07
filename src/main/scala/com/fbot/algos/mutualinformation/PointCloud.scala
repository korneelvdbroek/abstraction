package com.fbot.algos.mutualinformation

import TupleOps._
import com.fbot.common.immutable.{ArrayIndex, ImmutableArray, UnzippedMap}
import com.fbot.common.immutable.BooleanArrayMath._
import com.fbot.common.immutable.DoubleArrayMath._

import scala.annotation.tailrec

/**
  * Copyright (C) 5/30/2017 - REstore NV
  *
  *
  * note: use classes to render data type safe, do everything else functional
  */
case class PointCloud(points: ImmutableArray[Tuple], space: HyperSpace) {

  def kNearestBruteForce(pointIndices: ImmutableArray[ArrayIndex])(k: Int, currentTupleIndex: ArrayIndex): ImmutableArray[(ArrayIndex, Double)] = {
    val otherTuplesSortedByDistance = pointIndices
      .filterNot(_ == currentTupleIndex)
      .map(index => (index, distance(points(index), points(currentTupleIndex))))
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
  def enclosingBin(tupleIndex: ArrayIndex): UnitHyperCube = binnedPoints(tupleIndex)

  val pointsByBin: UnzippedMap[UnitHyperCube, ImmutableArray[ArrayIndex]] = points.indexRange.unzippedGroupBy(enclosingBin)

  def kNearest(k: Int, currentTupleIndex: ArrayIndex): ImmutableArray[ArrayIndex] = {

    val currentTuple = points(currentTupleIndex)

    @tailrec
    def kNearestInCube(kNearestCandidates: ImmutableArray[ArrayIndex],
                       remainingPointsByBin: UnzippedMap[UnitHyperCube, ImmutableArray[ArrayIndex]],
                       cube: HyperCube): ImmutableArray[(ArrayIndex, Double)] = {

      val newCandidateBins   = remainingPointsByBin.filterKeys(_ isIn cube)
      val newCandidatePoints = if (kNearestCandidates.isEmpty) {
        newCandidateBins.values.flatten.filterNot(_ == currentTupleIndex)
      } else {
        kNearestCandidates ++ newCandidateBins.values.flatten
      }

      println(s"cube = $cube; (${ newCandidatePoints.length})")
      if (newCandidatePoints.length > 100) {
        (0 to 100).foreach(i => print(s"${ points(newCandidatePoints(ArrayIndex(i))) }, "))
        println()
      }

      if (newCandidatePoints.length >= k) {
        val kNearestWithDistance = kNearestBruteForce(newCandidatePoints)(k, currentTupleIndex)
        val epsilon = kNearestWithDistance.map(_._2).last

        val growLeft = space.axes.map(axis => if ((currentTuple(axis) - cube.left.position(axis)) < epsilon) -space.unitCubeSize(axis) else 0d)
        val growRight = space.axes.map(axis => if ((cube.right.position(axis) - currentTuple(axis)) < epsilon) space.unitCubeSize(axis) else 0d)

        if (growLeft.forall(_ == 0d) && growRight.forall(_ == 0d)) {
          kNearestWithDistance
        } else {
          val newCube = cube.grow(growLeft, growRight)
          kNearestInCube(kNearestWithDistance.map(_._1), remainingPointsByBin.filterOut(newCandidateBins.filter), newCube)
        }

      } else {
        val newCube = cube.grow(-space.unitCubeSize, space.unitCubeSize)

        kNearestInCube(newCandidatePoints, remainingPointsByBin.filterOut(newCandidateBins.filter), newCube)
      }
    }

    // initialize
    val cube               = HyperCube.unit(points(currentTupleIndex))
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
