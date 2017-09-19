package com.fbot.algos.mutualinformation

import breeze.linalg.max
import breeze.numerics.{digamma, pow}
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
  val percentileLow = .10d
  val percentileHigh = .90d
  val massCube: Array[Double] = Array.range(0, 2 * dim).map(d => {
    val sortedCoordinate = points.map(tuple => tuple(d)).sortBy(x => x)
    val xLow = sortedCoordinate(ArrayIndex((percentileLow * length).floor.toInt))
    val xHigh = sortedCoordinate(ArrayIndex((percentileHigh * length).floor.toInt))

    val edgeLength = xHigh - xLow
    if (edgeLength == 0) 1d else edgeLength
  })

  // empirically we established that it is optimal to have ~100 points in a HyperSpaceUnit (for k = 10)
  val optimalPointsPerSpaceUnit = 100d
  val numberOfPointsInMassCube: Double = (percentileHigh - percentileLow) * length
  val unitSizes: Array[Double] = massCube.map(pow(optimalPointsPerSpaceUnit / numberOfPointsInMassCube, 1d / (2 * dim)) * _)
  val unitSizesX: Array[Double] = massCube.slice(0, dim).map(pow(optimalPointsPerSpaceUnit / numberOfPointsInMassCube, 1d / dim) * _)
  val unitSizesY: Array[Double] = massCube.slice(dim, 2 * dim).map(pow(optimalPointsPerSpaceUnit / numberOfPointsInMassCube, 1d / dim) * _)

  val space: HyperSpace = Space(ImmutableArray.indexRange(0, 2 * dim), unitSizes)
  val spaceX: HyperSpace = Space(ImmutableArray.indexRange(0, dim), unitSizesX)
  val spaceY: HyperSpace = Space(ImmutableArray.indexRange(dim, 2 * dim), unitSizesY)

  println(s"space  = $space")
  println(s"spaceX = $spaceX")
  println(s"spaceY = $spaceY")

  lazy val pointsBySpaceUnitPerSpace: Map[HyperSpace, Map[HyperSpaceUnit, ImmutableArray[ArrayIndex]]] = {
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

    println(s"done init...")
    Map(space -> pointsBySpaceUnit,
        spaceX -> pointsByProjectedSpaceXUnits,
        spaceY -> pointsByProjectedSpaceYUnits)
  }


  def MI(k: Int): Double = {
    val nxy: IndexedSeq[(Int, Int)] = (0 until length).map(ii => {
      val i = ArrayIndex(ii)

      val (kNearestIndices, t1) = Utils.timeIt {
//        kNearest(space)(k, i)
        kNearestBruteForce(space, ImmutableArray.indexRange(0, length).filterNot(_ == i))(k, points(i)).map(_._1)
      }

      val epsilonX = kNearestIndices.map(kNearestIndex => spaceX.distance(points(i), points(kNearestIndex))).max
      val epsilonY = kNearestIndices.map(kNearestIndex => spaceY.distance(points(i), points(kNearestIndex))).max

      val (x, t2) = Utils.timeIt {
//        (numberOfCloseByPoints(spaceX)(epsilonX, i), numberOfCloseByPoints(spaceY)(epsilonY, i))
        (numberOfCloseByPointsBruteForce(spaceX, ImmutableArray.indexRange(0, length))(epsilonX, points(i)),
          numberOfCloseByPointsBruteForce(spaceY, ImmutableArray.indexRange(0, length))(epsilonY, points(i)))
      }

      if (ii % 1 == 0) println(f"$i%12s:  ${ Utils.prettyPrintTime(t1) } // ${ Utils.prettyPrintTime(t2) }: $x")
      x
    })

    val ave = nxy.map(nxyi => digamma(nxyi._1) + digamma(nxyi._2)).sum / length

    max(digamma(k) - 1d / k - ave + digamma(length), 0d)
  }

}