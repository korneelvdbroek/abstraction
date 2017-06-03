package com.fbot.algos.mutualinformation

import TupleOps._
import com.fbot.common.immutable.{ArrayIndex, ImmutableArray}

/**
  * Copyright (C) 5/30/2017 - REstore NV
  *
  */
case class PointCloud(points: ImmutableArray[Tuple]) {

  def nearestBruteForce(k: Int, currentTupleIndex: ArrayIndex): (Int, ImmutableArray[ArrayIndex]) = {
    val currentTuple = points(currentTupleIndex)
    val otherTuplesSortedByDistance = points.indexRange
      .filterNot(_ == currentTupleIndex)
      .map(index => (index, distance(points(index), currentTuple)))
      .sortBy(_._2)


    var i = k
    while (i < otherTuplesSortedByDistance.length && otherTuplesSortedByDistance(ArrayIndex(k - 1))._2 ==  otherTuplesSortedByDistance(ArrayIndex(i))._2) {
      i += 1
    }

    (i, otherTuplesSortedByDistance.take(k).map(_._1))
  }

  /*
   New algo:
   1. OK def tuple => hyperCubeBin & store in map
   2. OK groupBy(tuple => hyperCubeBin)
   3. for center-point, find hyperCubeBin (use stored map) & find brute force the k-nearest (if number of points in hyperCube > k)
   4. if distance from farthest of k-nearest to center-point < edges of hyperCube (or number of points in hyperCube < k)
   5. include next shell of hyperCubeBins & go to 3 (only adding to the already found k-nearest the new points from the hyperCubeShell
   */

  val binning = UnitHyperCube(Array.range(0, points.dim).map(_ => 1.0), points)

  val pointsByBin: Map[HyperCubeBin, ImmutableArray[ArrayIndex]] = points.indexRange.groupBy(binning.enclosingBin)

  def kNearestBinned(k: Int, currentTupleIndex: ArrayIndex): (Int, Array[ArrayIndex]) = {

    ???
  }

}

object PointCloud {

  def apply(data: Tuple*): PointCloud = PointCloud(ImmutableArray(data.toArray))

  def apply(data: Array[Tuple]): PointCloud = PointCloud(ImmutableArray(data))

  def print[T](x: Array[T]): String = x.deep.mkString("Array(", ",", ")")

}
