package com.fbot.algos.mutualinformation

import breeze.linalg.max
import breeze.numerics.{abs, digamma, pow, sqrt}
import com.fbot.algos.nearestneighbors.NearestNeighbors
import com.fbot.common.data.Series
import com.fbot.common.fastcollections.FastTuple2Zipped._
import com.fbot.common.fastcollections.ImmutableArray
import com.fbot.common.fastcollections.index.ArrayIndex
import com.fbot.common.hyperspace.{HyperSpace, HyperSpaceUnit, Space, Tuple}
import com.fbot.main.Utils
import grizzled.slf4j.Logging

import scala.util.Random

/**
  * References:
  * + Estimating Mutual Information
  *   Alexander Kraskov, Harald Stogbauer, Peter Grassberger
  *   Phys. Rev. E 69, 066138
  *  	arXiv:cond-mat/0305641
  */
case class MutualInformation(dataX: ImmutableArray[Tuple], dataY: ImmutableArray[Tuple]) extends NearestNeighbors with Logging {

  val points: ImmutableArray[Tuple] = (dataX, dataY).map((x, y) => x ++ y)

  val length: Int = points.length

  val dim: Int = dataX(ArrayIndex(0)).dim

  // determine where the mass of the distribution is located
  val margin: Double = 0.0001d
  val massCubeVectors: ImmutableArray[(Double, Double)] = ImmutableArray.indexRange(0, 2 * dim).map(d => {
    val sortedCoordinate = points.map(tuple => tuple(d)).sortBy(x => x)
    val xLow = sortedCoordinate(ArrayIndex(0))
    val xHigh = sortedCoordinate(ArrayIndex(length - 1))

    val edgeLength = if (xHigh - xLow == 0) 1d else xHigh - xLow

    (xLow - margin * edgeLength, edgeLength * (1d + 2d*margin))
  })

  val numberOfPointsInMassCube: Double = length
  val optimalPointsPerSpaceUnit: Double = sqrt(length)   // optimal given our search strategy which is a tree of hyperSpaceUnits


  /**
    * Partitions the massCube into n^k (n+1)^(d-k) spaceUnits where N / n^k (n+1)^(d-k) = \sqrt{N} and n, k are integers.
    * Note that simply cutting up the massCube edges in n (real) spaceUnits where n^d = N / \sqrt{N} can result in too many spaceUnits
    * since effectively we have (n.floor + 1)^d spaceUnits which can be >> n^k (n+1)^(d-k)
    *
    * @param massCubeEdgeSize            edges of the cube that contains all data points
    * @param numberOfPointsInMassCube    number of data points
    * @param optimalPointsPerSpaceUnit   ideal number of data points per spaceUnit (in case of uniform distribution)
    * @return                            unitSizes defining the grid in the space
    */
  def getUnitSizes(massCubeEdgeSize: ImmutableArray[Double], numberOfPointsInMassCube: Double, optimalPointsPerSpaceUnit: Double): Tuple = {
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
    })._2.reverse

    info(s"${massCubeEdgeSize.length}d space: split into ${Tuple(massCubeEdgeSize.toArray) / Tuple(partitionVector)} space units")

    Tuple(partitionVector)
  }

  val unitSizes: Tuple = getUnitSizes(massCubeVectors.map(_._2), numberOfPointsInMassCube, optimalPointsPerSpaceUnit)
  val unitSizesX: Tuple = getUnitSizes(massCubeVectors.map(_._2).slice(0, dim), numberOfPointsInMassCube, optimalPointsPerSpaceUnit)
  val unitSizesY: Tuple = getUnitSizes(massCubeVectors.map(_._2).slice(dim, 2 * dim), numberOfPointsInMassCube, optimalPointsPerSpaceUnit)

  val space: HyperSpace = Space(ImmutableArray.indexRange(0, 2 * dim), massCubeVectors.map(_._1), unitSizes)
  val spaceX: HyperSpace = Space(ImmutableArray.indexRange(0, dim), massCubeVectors.map(_._1).slice(0, dim), unitSizesX)
  val spaceY: HyperSpace = Space(ImmutableArray.indexRange(dim, 2 * dim), massCubeVectors.map(_._1).slice(dim, 2 * dim), unitSizesY)

  def groupPointsBySpaceUnits(space: HyperSpace, points: ImmutableArray[Tuple]): (ImmutableArray[HyperSpaceUnit], ImmutableArray[ImmutableArray[ArrayIndex]]) = {
    // Slow initial computation
    val (pointsBySpaceUnitKeys, pointsBySpaceUnitValues) =
      points.indexRange.groupBy(index => space.hyperSpaceUnitAround(points(index))).unzip

    info(s"${space.dim}d space: actual ${pointsBySpaceUnitKeys.size} space units")

    (ImmutableArray(pointsBySpaceUnitKeys), ImmutableArray(pointsBySpaceUnitValues))
  }

  lazy val pointsBySpaceUnitPerSpace: Map[HyperSpace, (ImmutableArray[HyperSpaceUnit], ImmutableArray[ImmutableArray[ArrayIndex]])] = {
    Map(space -> groupPointsBySpaceUnits(space, points),
        spaceX -> groupPointsBySpaceUnits(spaceX, points),
        spaceY -> groupPointsBySpaceUnits(spaceY, points))
  }

  def MI(k: Int, tolerance: Double = 0.01): Double = {
    val sampleIndices = ImmutableArray.indexRange(0, length).map(i => (i, Random.nextDouble())).sortBy(_._2).mapWithIndex((x, index) => (x._1, index))

    val (mean, _): (Double, Double) = sampleIndices.foldLeftOrBreak((0d, 0d))((acc, doubleIndex) => {
      val (sampleIndex, countIndex) = doubleIndex
      val (oldMean, oldSumOfSquares) = acc

      val (kNearestIndices, t1) = Utils.timeIt {
        kNearest(space)(k, sampleIndex)
      }

      val epsilonX = kNearestIndices.map(kNearestIndex => spaceX.distance(points(sampleIndex), points(kNearestIndex))).max
      val epsilonY = kNearestIndices.map(kNearestIndex => spaceY.distance(points(sampleIndex), points(kNearestIndex))).max

      val (x, t2) = Utils.timeIt {
        digamma(numberOfCloseByPoints(spaceX)(epsilonX, sampleIndex)) + digamma(numberOfCloseByPoints(spaceY)(epsilonY, sampleIndex))
      }

      // Welford's online algorithm for sample mean and variance (https://en.wikipedia.org/wiki/Algorithms_for_calculating_variance)
      val n = countIndex.toInt + 1d
      val mean = ((n-1d)*oldMean + x) / n
      val sumOfSquares = oldSumOfSquares + (x-oldMean)*(x-mean)
      val standardErrorOfMean = if (n>1d) sqrt(sumOfSquares / ((n-1d)*n)) else Double.PositiveInfinity

      val MI = digamma(k) - 1d / k + digamma(length) - mean

      if (countIndex.toInt % 100 == 0) info(f"${countIndex.toInt}%7d ($sampleIndex%12s):  ${ Utils.prettyPrintTime(t1) } // ${ Utils.prettyPrintTime(t2) }: $MI%7.4f +/- $standardErrorOfMean%7.4f")

      ((mean, sumOfSquares), standardErrorOfMean < tolerance)
    })

    max(digamma(k) - 1d / k - mean + digamma(length), 0d)
  }

  def MIMax(k: Int): Double = {
    max(- digamma(k) - 1d / k + digamma(length), 0d)
  }

}