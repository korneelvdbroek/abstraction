package com.fbot.algos.mutualinformation

import com.fbot.common.fastcollections.ImmutableArray
import com.fbot.common.fastcollections.index.ArrayIndex
import com.fbot.common.hyperspace.TupleOps._
import com.fbot.common.hyperspace.{HyperCube, HyperSpace, Tuple, UnitHyperCube}

import scala.annotation.tailrec

/**
  *
  */
case class PointCloud(points: ImmutableArray[Tuple], space: HyperSpace) {

  def kNearestBruteForce(pointIndices: ImmutableArray[ArrayIndex])(k: Int, currentTuple: Tuple): ImmutableArray[(ArrayIndex, Double)] = {
    val otherTuplesSortedByDistance = pointIndices
      .map(index => (index, distance(points(index), currentTuple)))
      .partialSort(k, (el1, el2) => el1._2 < el2._2)

    otherTuplesSortedByDistance
  }


  lazy val binnedPoints: ImmutableArray[UnitHyperCube] = points.map(space.findEnclosingUnitHyperCube)

  lazy val pointsByBin: Map[UnitHyperCube, ImmutableArray[ArrayIndex]] = points.indexRange.groupBy(binnedPoints(_))

  def kNearest(k: Int, currentTupleIndex: ArrayIndex): ImmutableArray[ArrayIndex] = {

    val currentTuple = points(currentTupleIndex)

    @tailrec
    def kNearestInCube(kNearestCandidates: ImmutableArray[ArrayIndex],
                       cube: HyperCube, newUnitCubes: ImmutableArray[UnitHyperCube]): ImmutableArray[(ArrayIndex, Double)] = {

      val newPointsInNewBins = newUnitCubes.map(pointsByBin.getOrElse(_, ImmutableArray.empty[ArrayIndex])).flatten

      val newCandidatePoints = if (kNearestCandidates.isEmpty) {
        newPointsInNewBins.filterNot(_ == currentTupleIndex)
      } else {
        kNearestCandidates ++ newPointsInNewBins
      }

      //println(f"$cube%60s: #candidates = ${kNearestCandidates.length }%3d + ${newCandidateBins.values.flatten.length }%3d")

      if (newCandidatePoints.length >= k) {
        val kNearestWithDistance = kNearestBruteForce(newCandidatePoints)(k, currentTuple)
        val epsilon = kNearestWithDistance.last._2

        val growLeft = space.axes.map(axis => if ((currentTuple(axis) - cube.left.repr(axis) * space.unitCubeSize(axis)) < epsilon) -1L else 0L)
        val growRight = space.axes.map(axis => if ((cube.right.repr(axis) * space.unitCubeSize(axis) - currentTuple(axis)) < epsilon) 1L else 0L)

        if (growLeft.forall(_ == 0L) && growRight.forall(_ == 0L)) {
          kNearestWithDistance
        } else {
          val (newCube, newUnitCubes) = cube.grow(growLeft, growRight)
          kNearestInCube(kNearestWithDistance.map(_._1), newCube, newUnitCubes)
        }

      } else {
        val (newCube, newUnitCubes) = cube.grow(Array.fill[Long](space.dim)(-1L), Array.fill[Long](space.dim)(1L))

        kNearestInCube(newCandidatePoints, newCube, newUnitCubes)
      }
    }

    // initialize
    val unitCube = space.unitCube(currentTuple)
    val kNearestCandidates = ImmutableArray.empty[ArrayIndex]
    kNearestInCube(kNearestCandidates, unitCube, ImmutableArray(unitCube)).map(_._1)
  }


}

object PointCloud {

  def print[T](x: Array[T]): String = x.deep.mkString("Array(", ",", ")")

}

