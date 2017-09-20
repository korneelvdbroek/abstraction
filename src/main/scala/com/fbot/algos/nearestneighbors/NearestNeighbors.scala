package com.fbot.algos.nearestneighbors

import com.fbot.common.fastcollections.ImmutableArray
import com.fbot.common.fastcollections.index.ArrayIndex
import com.fbot.common.hyperspace._
import com.fbot.main.Utils

import scala.annotation.tailrec
//import ImmutableArray._

/**
  *
  */
trait NearestNeighbors {

  def points: ImmutableArray[Tuple]

  def pointsBySpaceUnitPerSpace: Map[HyperSpace, ImmutableArray[(HyperSpaceUnit, ImmutableArray[ArrayIndex])]]

  def pointsBySpaceUnitPerSpaceKeys: Map[HyperSpace, ImmutableArray[HyperSpaceUnit]]
  def pointsBySpaceUnitPerSpaceValues: Map[HyperSpace, ImmutableArray[ImmutableArray[ArrayIndex]]]

  def kNearestBruteForce(space: HyperSpace, pointSubsetIndices: ImmutableArray[ArrayIndex])
                        (k: Int, currentTuple: Tuple): ImmutableArray[(ArrayIndex, Double)] = {
    val (x, t0) = Utils.timeIt{
      pointSubsetIndices
      .map(index => (index, space.distance(points(index), currentTuple)))
      .partialSort(k, (el1, el2) => el1._2 < el2._2)
    }
    println(f"   - Brute Force t = ${Utils.prettyPrintTime(t0)}   (length = ${pointSubsetIndices.length}")
    x
  }

  def kNearest(space: HyperSpace)(k: Int, centerTupleIndex: ArrayIndex): ImmutableArray[ArrayIndex] = {

    val centerTuple = points(centerTupleIndex)
    //val pointsBySpaceUnit = pointsBySpaceUnitPerSpace(space)

    val pointsBySpaceUnitKeys = pointsBySpaceUnitPerSpaceKeys(space)
    val pointsBySpaceUnitValues = pointsBySpaceUnitPerSpaceValues(space)



    @tailrec
    def kNearestInCube(kNearestCandidates: ImmutableArray[ArrayIndex],
                       cube: HyperCube, oldCube: HyperCube): ImmutableArray[(ArrayIndex, Double)] = {

      // when number of hyperSpaceUnits: 3^d << N / 100 = number of entries in the Map pointsBySpaceUnitPerSpace(space)
//      val pointsInNewUnitCubes = cubeSideUnitCubes.map(pointsBySpaceUnit.getOrElse(_, ImmutableArray.empty[ArrayIndex])).flatten

//      println(s"trade-off: hyper-space-cubes = ${cube.minus(oldCube).length}   vs  entries in map = ${pointsBySpaceUnitPerSpace(space).length} ")
      // when number of hyperSpaceUnits: 3^d >> N / 100 = number of entries in the Map pointsBySpaceUnitPerSpace(space)
      val (pointsInNewUnitCubesNonFlat, t1) = Utils.timeIt {
        pointsBySpaceUnitKeys.mapWithIndex((hyperSpaceUnit, index) => {
          //val (hyperSpaceUnit, index) = hyperSpaceUnitWithIndex

          if (cube.contains(hyperSpaceUnit) && !oldCube.contains(hyperSpaceUnit)) {
            pointsBySpaceUnitValues(index)
          } else {
            ImmutableArray.empty[ArrayIndex]
          }
        })
      }
      println(f"   - PickPoints  t = ${Utils.prettyPrintTime(t1)}")


      val (pointsInNewUnitCubes, t2) = Utils.timeIt{
        pointsInNewUnitCubesNonFlat.flatten
      }
      println(f"   - Flatten     t = ${Utils.prettyPrintTime(t2)}")

      val newCandidatePoints = if (kNearestCandidates.isEmpty) {
        pointsInNewUnitCubes.filterNot(_ == centerTupleIndex)
      } else {
        kNearestCandidates ++ pointsInNewUnitCubes
      }

      //println(f"$cube%60s: #candidates = ${kNearestCandidates.length }%3d + ${newCandidateBins.values.flatten.length }%3d")

      if (newCandidatePoints.length >= k) {
        val kNearestWithDistance = kNearestBruteForce(space, newCandidatePoints)(k, centerTuple)
        val epsilon = kNearestWithDistance.last._2


        val newCube = cube.growCubeSidesToIncludeDistanceAround(space)(epsilon, centerTuple)
//        val newHyperSpaceUnits = newCube.minus(cube)  // expensive in high dim

//        if (newHyperSpaceUnits.isEmpty) {
        if (newCube == cube) {
          kNearestWithDistance
        } else {
          kNearestInCube(kNearestWithDistance.map(_._1), newCube, cube)
        }

      } else {
        val newCube = cube.grow(ImmutableArray.fill[Long](space.dim)(-1L), ImmutableArray.fill[Long](space.dim)(1L))
//        val newHyperSpaceUnits = newCube.minus(cube)  // expensive in high dim

        kNearestInCube(newCandidatePoints, newCube, cube)
      }
    }

    // initialize
    val spaceUnitAroundTuple = space.hyperSpaceUnitAround(centerTuple)
    val kNearestCandidates = ImmutableArray.empty[ArrayIndex]
    kNearestInCube(kNearestCandidates, HyperCube.from(spaceUnitAroundTuple), HyperCube.empty(spaceUnitAroundTuple))
      .map(_._1)
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

    // when number of hyperSpaceUnits: 3^d << N / 100 = number of entries in the Map pointsBySpaceUnitPerSpace(space)
//    val closeByHyperSpaceUnits = cube
//      .hyperSpaceUnits  // expensive in high dim
//
//    val potentialCloseByPoints = closeByHyperSpaceUnits.map(pointsBySpaceUnitPerSpace(space).getOrElse(_, ImmutableArray.empty[ArrayIndex])).flatten

    // when number of hyperSpaceUnits: 3^d >> N / 100 = number of entries in the Map pointsBySpaceUnitPerSpace(space)
    val potentialCloseByPoints = pointsBySpaceUnitPerSpace(space).map(hyperSpaceUnitWithPoints => {
      val (hyperSpaceUnit, pointIndices) = hyperSpaceUnitWithPoints

      if (cube.contains(hyperSpaceUnit)) {
        pointIndices
      } else {
        ImmutableArray.empty[ArrayIndex]
      }
    }).flatten


    numberOfCloseByPointsBruteForce(space, potentialCloseByPoints)(distance, centerTuple)
  }

}