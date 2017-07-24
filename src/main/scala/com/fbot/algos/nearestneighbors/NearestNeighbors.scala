package com.fbot.algos.nearestneighbors

import com.fbot.common.data.DataPoints
import com.fbot.common.fastcollections.ImmutableArray
import com.fbot.common.fastcollections.index.ArrayIndex
import com.fbot.common.hyperspace._

import scala.annotation.tailrec

/**
  *
  */
trait NearestNeighbors extends DataPoints {

  // Slow initial computation
  lazy val pointsBySpaceUnit: Map[HyperSpaceUnit, ImmutableArray[ArrayIndex]] = {
    val binnedPoints: ImmutableArray[HyperSpaceUnit] = points.map(space.hyperSpaceUnitAround)

    points.indexRange.groupBy(binnedPoints(_))
  }

  def kNearestBruteForce(pointIndices: ImmutableArray[ArrayIndex])(k: Int, currentTuple: Tuple): ImmutableArray[(ArrayIndex, Double)] = {
    pointIndices
      .map(index => (index, space.distance(points(index), currentTuple)))
      .partialSort(k, (el1, el2) => el1._2 < el2._2)
  }

  def kNearest(k: Int, centerTupleIndex: ArrayIndex): ImmutableArray[ArrayIndex] = {

    val centerTuple = points(centerTupleIndex)

    @tailrec
    def kNearestInCube(kNearestCandidates: ImmutableArray[ArrayIndex],
                       cube: HyperCube, cubeSideUnitCubes: ImmutableArray[HyperSpaceUnit]): ImmutableArray[(ArrayIndex, Double)] = {

      val pointsInNewUnitCubes = cubeSideUnitCubes.map(pointsBySpaceUnit.getOrElse(_, ImmutableArray.empty[ArrayIndex])).flatten

      val newCandidatePoints = if (kNearestCandidates.isEmpty) {
        pointsInNewUnitCubes.filterNot(_ == centerTupleIndex)
      } else {
        kNearestCandidates ++ pointsInNewUnitCubes
      }

      //println(f"$cube%60s: #candidates = ${kNearestCandidates.length }%3d + ${newCandidateBins.values.flatten.length }%3d")

      if (newCandidatePoints.length >= k) {
        val kNearestWithDistance = kNearestBruteForce(newCandidatePoints)(k, centerTuple)
        val epsilon = kNearestWithDistance.last._2


        val newCube = cube.growCubeSidesToIncludeDistanceAround(epsilon, centerTuple, space.unitCubeSizes, space.normalCoordinate)
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
    kNearestInCube(kNearestCandidates, HyperCube.from(spaceUnitAroundTuple), ImmutableArray(spaceUnitAroundTuple))
      .map(_._1)
  }


  def numberOfCloseByPointsBruteForce(pointIndices: ImmutableArray[ArrayIndex])(distance: Double, centerTuple: Tuple): Int = {
    pointIndices.count(index => space.distance(points(index), centerTuple) < distance)
  }


  def numberOfCloseByPoints(distance: Double, centerTupleIndex: ArrayIndex): Int = {
    val centerTuple = points(centerTupleIndex)

    val cube = HyperCube.from(space.hyperSpaceUnitAround(centerTuple))
    val closeByHyperSpaceUnits = cube.growCubeSidesToIncludeDistanceAround(distance, centerTuple, space.unitCubeSizes, space.normalCoordinate)
      .hyperSpaceUnits

    val potentialCloseByPoints = closeByHyperSpaceUnits.map(pointsBySpaceUnit.getOrElse(_, ImmutableArray.empty[ArrayIndex])).flatten

    numberOfCloseByPointsBruteForce(potentialCloseByPoints)(distance, centerTuple)
  }

}


