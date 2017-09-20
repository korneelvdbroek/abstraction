package com.fbot.algos.mutualinformation

import breeze.linalg.max
import breeze.numerics.{digamma, pow, sqrt}
import com.fbot.algos.nearestneighbors.NearestNeighbors
import com.fbot.common.fastcollections.FastTuple2Zipped._
import com.fbot.common.fastcollections.ImmutableArray
import com.fbot.common.fastcollections.index.ArrayIndex
import com.fbot.common.hyperspace.{HyperSpace, HyperSpaceUnit, Space, Tuple}
import com.fbot.main.Utils

/**
  * References:
  * + Estimating Mutual Information
  *   Alexander Kraskov, Harald Stogbauer, Peter Grassberger
  *   Phys. Rev. E 69, 066138
  *  	arXiv:cond-mat/0305641
  */
case class MutualInformation(dataX: ImmutableArray[Tuple], dataY: ImmutableArray[Tuple]) extends NearestNeighbors {

  val points: ImmutableArray[Tuple] = (dataX, dataY).map((x, y) => x ++ y)

  val length: Int = points.length

  val dim: Int = dataX(ArrayIndex(0)).dim

  // determine where the mass of the distribution is located
  val massCubeEdgeSize: ImmutableArray[Double] = ImmutableArray.range(0, 2*dim).map(d => {
    val sortedCoordinate = points.map(tuple => tuple(d)).sortBy(x => x)
    val xLow = sortedCoordinate(ArrayIndex(0))
    val xHigh = sortedCoordinate(ArrayIndex(length - 1))

    val edgeLength = xHigh - xLow
    if (edgeLength == 0) 1d else edgeLength
  })

  println(massCubeEdgeSize)


  val numberOfPointsInMassCube: Double = length
  val optimalPointsPerSpaceUnit: Double = length  // sqrt(length)

  // N / n^k (n+1)^(d-k) = \sqrt{N}
  // where n^k (n+1)^(d-k) = number of SpaceUnits we cut the space into (n, k are integer)
  def partitionMassCube(massCubeEdgeSize: ImmutableArray[Double], numberOfPointsInMassCube: Double, optimalPointsPerSpaceUnit: Double): ImmutableArray[Double] = {
    val dim = massCubeEdgeSize.length
    val cutFactor = pow(numberOfPointsInMassCube / optimalPointsPerSpaceUnit, 1d / dim)
    val lowerCutFactor = cutFactor.floor

    val lowBoundFactor = lowerCutFactor / cutFactor
    val highBoundFactor = (lowerCutFactor + 1d) / cutFactor

    val partitionVector = massCubeEdgeSize.foldLeft((1d, List.empty[Double]))((acc, edge) => {
      val (bound, partition) = acc

      if (bound * highBoundFactor < 1d) {
        (bound * highBoundFactor, (edge / (lowerCutFactor + 1d)) :: partition)
      } else {
        (bound * lowBoundFactor, (edge / lowerCutFactor) :: partition)
      }
    })._2

    ImmutableArray(partitionVector.reverse)
  }

  val unitSizes: ImmutableArray[Double] = partitionMassCube(massCubeEdgeSize, numberOfPointsInMassCube, optimalPointsPerSpaceUnit)
  val unitSizesX: ImmutableArray[Double] = partitionMassCube(massCubeEdgeSize.slice(0, dim), numberOfPointsInMassCube, optimalPointsPerSpaceUnit)
  val unitSizesY: ImmutableArray[Double] = partitionMassCube(massCubeEdgeSize.slice(dim, 2 * dim), numberOfPointsInMassCube, optimalPointsPerSpaceUnit)

  println(s"unitSizes = $unitSizes")

  val space: HyperSpace = Space(ImmutableArray.indexRange(0, 2 * dim), unitSizes)
  val spaceX: HyperSpace = Space(ImmutableArray.indexRange(0, dim), unitSizesX)
  val spaceY: HyperSpace = Space(ImmutableArray.indexRange(dim, 2 * dim), unitSizesY)

  lazy val pointsBySpaceUnitPerSpace: Map[HyperSpace, ImmutableArray[(HyperSpaceUnit, ImmutableArray[ArrayIndex])]] = {
    // Slow initial computation
    val pointsBySpaceUnit: Map[HyperSpaceUnit, ImmutableArray[ArrayIndex]] =
      points.indexRange.groupBy(index => space.hyperSpaceUnitAround(points(index)))

    // Cut down on initialization calculation, if spaceX, spaceY are projected spaces:
    //    val spaceUnits = ImmutableArray(pointsBySpaceUnit.keys)
    //
    //    val pointsByProjectedSpaceXUnits: Map[HyperSpaceUnit, ImmutableArray[ArrayIndex]] =
    //      spaceUnits.groupBy(_.project(spaceX)).mapValues(_.flatMap(pointsBySpaceUnit(_).toArray))
    //
    //    val pointsByProjectedSpaceYUnits: Map[HyperSpaceUnit, ImmutableArray[ArrayIndex]] =
    //      spaceUnits.groupBy(_.project(spaceY)).mapValues(_.flatMap(pointsBySpaceUnit(_).toArray))

    val pointsByProjectedSpaceXUnits: Map[HyperSpaceUnit, ImmutableArray[ArrayIndex]] =
      points.indexRange.groupBy(index => spaceX.hyperSpaceUnitAround(points(index)))

    val pointsByProjectedSpaceYUnits: Map[HyperSpaceUnit, ImmutableArray[ArrayIndex]] =
      points.indexRange.groupBy(index => spaceY.hyperSpaceUnitAround(points(index)))

    Map(space -> ImmutableArray(pointsBySpaceUnit.toArray),
        spaceX -> ImmutableArray(pointsByProjectedSpaceXUnits.toArray),
        spaceY -> ImmutableArray(pointsByProjectedSpaceYUnits.toArray))
  }

  val pointsBySpaceUnitPerSpaceKeys: Map[HyperSpace, ImmutableArray[HyperSpaceUnit]] = pointsBySpaceUnitPerSpace.mapValues(array => {
    array.map(_._1)
  })
  val pointsBySpaceUnitPerSpaceValues: Map[HyperSpace, ImmutableArray[ImmutableArray[ArrayIndex]]] = pointsBySpaceUnitPerSpace.mapValues(array => {
    array.map(_._2)
  })

  println(s"space = ${space}")
  println(s"keys for space   = ${ pointsBySpaceUnitPerSpaceKeys(space).length }")

  //println(s"values for space = ${ pointsBySpaceUnitPerSpaceValues(space).map(_.length) }")

  def MI(k: Int): Double = {
    val nxy: IndexedSeq[(Int, Int)] = (0 until length).map(ii => {
      val i = ArrayIndex(ii)

      val (kNearestIndices, t1) = Utils.timeIt {
        kNearest(space)(k, i)
//        kNearestBruteForce(space, ImmutableArray.indexRange(0, length).filterNot(_ == i))(k, points(i)).map(_._1)
      }

      val epsilonX = kNearestIndices.map(kNearestIndex => spaceX.distance(points(i), points(kNearestIndex))).max
      val epsilonY = kNearestIndices.map(kNearestIndex => spaceY.distance(points(i), points(kNearestIndex))).max

      val (x, t2) = Utils.timeIt {
        (numberOfCloseByPoints(spaceX)(epsilonX, i), numberOfCloseByPoints(spaceY)(epsilonY, i))
//        (numberOfCloseByPointsBruteForce(spaceX, ImmutableArray.indexRange(0, length))(epsilonX, points(i)),
//          numberOfCloseByPointsBruteForce(spaceY, ImmutableArray.indexRange(0, length))(epsilonY, points(i)))
      }

      if (ii % 1 == 0) println(f"$i%12s:  ${ Utils.prettyPrintTime(t1) } // ${ Utils.prettyPrintTime(t2) }: $x")
      x
    })

    val ave = nxy.map(nxyi => digamma(nxyi._1) + digamma(nxyi._2)).sum / length

    max(digamma(k) - 1d / k - ave + digamma(length), 0d)
  }

}