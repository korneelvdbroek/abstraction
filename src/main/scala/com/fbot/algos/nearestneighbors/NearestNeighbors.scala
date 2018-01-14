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

  def pointsBySpaceUnitPerSpace: Map[HyperSpace, (ImmutableArray[HyperSpaceUnit], ImmutableArray[ImmutableArray[Int]])]


  def kNearestBruteForce(space: HyperSpace, pointSubsetIndices: ImmutableArray[ArrayIndex])
                        (k: Int, currentTuple: Tuple): ImmutableArray[(Int, Double)] = {
    pointSubsetIndices
      .map(index => (index, space.distance(points(index), currentTuple)))
      .partialSort(k, (el1, el2) => el1._2 < el2._2)
  }


  def kNearest(space: HyperSpace)(k: Int, centerTupleIndex: ArrayIndex): ImmutableArray[Int] = {

    val centerTuple = points(centerTupleIndex)
    val (pointsBySpaceUnitKeys, pointsBySpaceUnitValues) = pointsBySpaceUnitPerSpace(space)


    @tailrec
    def kNearestInCube(kNearestCandidates: ImmutableArray[Int],
                       cube: HyperCube, oldCube: HyperCube): ImmutableArray[(Int, Double)] = {

      val pointsInNewUnitCubesUnflattened: ImmutableArray[ImmutableArray[Int]] = pointsBySpaceUnitKeys.mapWithIndex((hyperSpaceUnit, index) => {
        if (cube.contains(hyperSpaceUnit) && !oldCube.contains(hyperSpaceUnit)) {
          pointsBySpaceUnitValues(index)
        } else {
          ImmutableArray.empty[Int]
        }
      })
      val pointsInNewUnitCubes: ImmutableArray[Int] = pointsInNewUnitCubesUnflattened
        .flatten[Int, ImmutableArray[Int]]

      val newCandidatePoints = if (kNearestCandidates.isEmpty) {
        pointsInNewUnitCubes.filterNot(_ == centerTupleIndex)
      } else {
        kNearestCandidates ++ pointsInNewUnitCubes
      }

      if (newCandidatePoints.length >= k) {
        val kNearestWithDistance = kNearestBruteForce(space, newCandidatePoints)(k, centerTuple)
        val epsilon = kNearestWithDistance.last._2

        val newCube = cube.growCubeSidesToIncludeDistanceAround(space)(epsilon, centerTuple)

        if (newCube == cube) {
          kNearestWithDistance
        } else {
          kNearestInCube(kNearestWithDistance.map(_._1), newCube, cube)
        }

      } else {
        val newCube = cube.grow(ImmutableArray.fill[Long](space.dim)(-1L), ImmutableArray.fill[Long](space.dim)(1L))

        kNearestInCube(newCandidatePoints, newCube, cube)
      }
    }

    // initialize
    val spaceUnitAroundTuple = space.hyperSpaceUnitAround(centerTuple)
    val kNearestCandidates = ImmutableArray.empty[Int]
    kNearestInCube(kNearestCandidates, HyperCube.from(spaceUnitAroundTuple), HyperCube.empty(spaceUnitAroundTuple))
      .map(_._1)
  }


  def numberOfCloseByPointsBruteForce(space: HyperSpace, pointSubsetIndices: ImmutableArray[Int])
                                     (distance: Double, centerTuple: Tuple): Int = {
    // minus 1 to exclude the self (so self should be in pointSubsetIndices)
    pointSubsetIndices.count(index => space.distance(points(index), centerTuple) <= distance) - 1
  }


  /**
    * Number of points x_j which have ||x_j - x_i|| <= distance and j != i
    */
  def numberOfCloseByPoints(space: HyperSpace)(distance: Double, centerTupleIndex: Int): Int = {
    val centerTuple = points(centerTupleIndex)

    val cube = HyperCube.from(space.hyperSpaceUnitAround(centerTuple)).growCubeSidesToIncludeDistanceAround(space)(distance, centerTuple)

    val (pointsBySpaceUnitKeys, pointsBySpaceUnitValues) = pointsBySpaceUnitPerSpace(space)
    val potentialCloseByPoints: ImmutableArray[Int] = pointsBySpaceUnitKeys.mapWithIndex((hyperSpaceUnit, index) => {
      if (cube.contains(hyperSpaceUnit)) {
        pointsBySpaceUnitValues(index)
      } else {
        ImmutableArray.empty[Int]
      }
    }).flatten[Int, ImmutableArray[Int]]


    numberOfCloseByPointsBruteForce(space, potentialCloseByPoints)(distance, centerTuple)
  }

}