package com.fbot.algos.mutualinformation

import com.fbot.common.fastcollections.ImmutableArray
import com.fbot.common.fastcollections.index.ArrayIndex
import com.fbot.common.hyperspace._

import scala.annotation.tailrec

/**
  *
  */
case class PointCloud(points: ImmutableArray[Tuple], space: HyperSpace) {

  def kNearestBruteForce(pointIndices: ImmutableArray[ArrayIndex])(k: Int, currentTuple: Tuple): ImmutableArray[(ArrayIndex, Double)] = {
    val otherTuplesSortedByDistance = pointIndices
      .map(index => (index, space.distance(points(index), currentTuple)))
      .partialSort(k, (el1, el2) => el1._2 < el2._2)

    otherTuplesSortedByDistance
  }


  lazy val binnedPoints: ImmutableArray[HyperSpaceUnit] = points.map(space.hyperSpaceUnitAround)

  lazy val pointsByBin: Map[HyperSpaceUnit, ImmutableArray[ArrayIndex]] = points.indexRange.groupBy(binnedPoints(_))

  def kNearest(k: Int, centerTupleIndex: ArrayIndex): ImmutableArray[ArrayIndex] = {

    val centerTuple = points(centerTupleIndex)

    @tailrec
    def kNearestInCube(kNearestCandidates: ImmutableArray[ArrayIndex],
                       cube: HyperCube, cubeSideUnitCubes: ImmutableArray[HyperSpaceUnit]): ImmutableArray[(ArrayIndex, Double)] = {

      val pointsInNewUnitCubes = cubeSideUnitCubes.map(pointsByBin.getOrElse(_, ImmutableArray.empty[ArrayIndex])).flatten

      val newCandidatePoints = if (kNearestCandidates.isEmpty) {
        pointsInNewUnitCubes.filterNot(_ == centerTupleIndex)
      } else {
        kNearestCandidates ++ pointsInNewUnitCubes
      }

      //println(f"$cube%60s: #candidates = ${kNearestCandidates.length }%3d + ${newCandidateBins.values.flatten.length }%3d")

      if (newCandidatePoints.length >= k) {
        val kNearestWithDistance = kNearestBruteForce(newCandidatePoints)(k, centerTuple)
        val epsilon = kNearestWithDistance.last._2


        val newCube = cube.growCubeSidesToIncludeDistanceAround(epsilon, centerTuple, space.unitCubeSize, space.normalCoordinate)
        val newHyperSpaceUnits = newCube.minus(cube)

        if (newHyperSpaceUnits.isEmpty) {
          kNearestWithDistance
        } else {
          kNearestInCube(kNearestWithDistance.map(_._1), newCube, newHyperSpaceUnits)
        }

      } else {
        val newCube = cube.grow(ImmutableArray.fill[Long](space.dim)(-1L), ImmutableArray.fill[Long](space.dim)(1L))
        val newHyperSpaceUnits = newCube.minus(cube)

        kNearestInCube(newCandidatePoints, newCube, newHyperSpaceUnits)
      }
    }

    // initialize
    val spaceUnitAroundTuple = space.hyperSpaceUnitAround(centerTuple)
    val kNearestCandidates = ImmutableArray.empty[ArrayIndex]
    kNearestInCube(kNearestCandidates, HyperCube.from(spaceUnitAroundTuple), ImmutableArray(spaceUnitAroundTuple)).map(_._1)
  }

  def numberOfPointsWithin(distance: Double, currentTupleIndex: ArrayIndex): Long = {
    // TODO:
    /**
      * -1. do preparatory work (see HyperCube --> growCubeSidesToIncludeDistance())
      * 0.  get cube of currentTupleIndex
      * 1.  check distance to each side [make this part of HyperCube class]
      * 2.  grow the cube accordingly, repeat 1 & 2 until cube grows no more   [no need for unitHyperCubes, so split it off]
      * 3.  get all points of the found unit cubes (via pointsByBin), and .count(predicate)
      */
    ???
  }

}

object PointCloud {

  def print[T](x: Array[T]): String = x.deep.mkString("Array(", ",", ")")

}

