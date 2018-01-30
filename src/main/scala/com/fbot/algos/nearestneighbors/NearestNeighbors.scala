package com.fbot.algos.nearestneighbors

import com.fbot.common.fastcollections.{ImmutableArray, ImmutableTupleArray, Tuple}
import com.fbot.common.fastcollections._
import com.fbot.common.hyperspace._

import scala.annotation.tailrec

/**
  *
  */
trait NearestNeighbors {

  // TODO: refactor to unpack the Tuple to ImmutableArray[ImmutableArray[Double]]
  def points: ImmutableTupleArray

  def pointsBySpaceUnitPerSpace: Map[HyperSpace, (ImmutableArray[HyperSpaceUnit], ImmutableArray[ImmutableArray[ArrayIndex]])]


  def kNearestBruteForce(space: HyperSpace, pointSubsetIndices: ImmutableArray[ArrayIndex])
                        (k: Int, currentTuple: Tuple): (ImmutableArray[ArrayIndex], ImmutableArray[Double]) = {
    val (sortedSubIndices, sortedDistances) = pointSubsetIndices
      .mapToNewType(index => space.distance(points(index), currentTuple))
      .partialSort(k)

    (sortedSubIndices.map(subIndex => pointSubsetIndices(subIndex)), sortedDistances)
  }


  def kNearest(space: HyperSpace)(k: Int, centerTupleIndex: ArrayIndex): ImmutableArray[ArrayIndex] = {

    val centerTuple = points(centerTupleIndex)
    val (pointsBySpaceUnitKeys, pointsBySpaceUnitValues) = pointsBySpaceUnitPerSpace(space)


    @tailrec
    def kNearestInCube(kNearestCandidates: ImmutableArray[ArrayIndex],
                       cube: HyperCube, oldCube: HyperCube): (ImmutableArray[ArrayIndex], ImmutableArray[Double]) = {

      val pointsInNewUnitCubes: ImmutableArray[ArrayIndex] = pointsBySpaceUnitKeys.flatMapWithIndex((hyperSpaceUnit, index) => {
        if (cube.contains(hyperSpaceUnit) && !oldCube.contains(hyperSpaceUnit)) {
          pointsBySpaceUnitValues(index)
        } else {
          ImmutableArray.empty[ArrayIndex]
        }
      })

      val newCandidatePoints = if (kNearestCandidates.isEmpty) {
        pointsInNewUnitCubes.filterNot(_ == centerTupleIndex)
      } else {
        kNearestCandidates ++ pointsInNewUnitCubes
      }

      if (newCandidatePoints.length >= k) {
        val kNearestWithDistance = kNearestBruteForce(space, newCandidatePoints)(k, centerTuple)
        val epsilon = kNearestWithDistance._2.last

        val newCube = cube.growCubeSidesToIncludeDistanceAround(space)(epsilon, centerTuple)

        if (newCube == cube) {
          kNearestWithDistance
        } else {
          kNearestInCube(kNearestWithDistance._1, newCube, cube)
        }

      } else {
        val newCube = cube.grow(ImmutableArray.fill[Long](space.dim)(-1L), ImmutableArray.fill[Long](space.dim)(1L))

        kNearestInCube(newCandidatePoints, newCube, cube)
      }
    }

    // initialize
    val spaceUnitAroundTuple = space.hyperSpaceUnitAround(centerTuple)
    val kNearestCandidates = ImmutableArray.empty[ArrayIndex]
    kNearestInCube(kNearestCandidates, HyperCube.from(spaceUnitAroundTuple), HyperCube.empty(spaceUnitAroundTuple))._1
  }


  def numberOfCloseByPointsBruteForce(space: HyperSpace, pointSubsetIndices: ImmutableArray[ArrayIndex])
                                     (distance: Double, centerTuple: Tuple): Int = {
    // minus 1 to exclude the self (so self should be in pointSubsetIndices)
    pointSubsetIndices.count(index => space.distance(points(index), centerTuple) <= distance) - 1
  }


  /**
    * Number of points x_j which have ||x_j - x_i|| <= distance and j != i
    */
  def numberOfCloseByPoints(space: HyperSpace)(distance: Double, centerTupleIndex: ArrayIndex): Int = {
    val centerTuple = points(centerTupleIndex)

    val cube = HyperCube.from(space.hyperSpaceUnitAround(centerTuple)).growCubeSidesToIncludeDistanceAround(space)(distance, centerTuple)

    val (pointsBySpaceUnitKeys, pointsBySpaceUnitValues) = pointsBySpaceUnitPerSpace(space)
    val potentialCloseByPoints: ImmutableArray[ArrayIndex] = pointsBySpaceUnitKeys.flatMapWithIndex((hyperSpaceUnit, index) => {
      if (cube.contains(hyperSpaceUnit)) {
        pointsBySpaceUnitValues(index)
      } else {
        ImmutableArray.empty[ArrayIndex]
      }
    })


    numberOfCloseByPointsBruteForce(space, potentialCloseByPoints)(distance, centerTuple)
  }

}