package com.fbot.algos.mutualinformation

import TupleOps._
import TupleIndex.index._
import Ordering.Implicits._

import scala.annotation.tailrec

/**
  * Copyright (C) 5/30/2017 - REstore NV
  *
  */
class TupleArray(tuples: Array[Tuple]) {

  def length: Int = tuples.length

  def apply(tupleIndex: TupleIndex): Tuple = tuples(tupleIndex.i)

  def dim: Int = {
    if (tuples.length > 0) tuples(0).dim else 0
  }

  override def toString: String = {
    tuples.mkString("[", ",\n", "]")
  }

  def indexRange: Array[TupleIndex] = Array.range(0, tuples.length).map(TupleIndex(_))

}


object TupleArray {

  def apply(data: Tuple*): TupleArray = new TupleArray(data.toArray)

}


class Dots(tuples: TupleArray) {
  val sortedData: Array[(Array[TupleIndex], Array[Int])] = {
    def indexSortAndRank(axis: Int): (Array[TupleIndex], Array[Int]) = {
      val sortedIndex = tuples.indexRange.sortWith(tuples(_)(axis) < tuples(_)(axis))
      val rank = Array.range(0, tuples.length).sortWith((i,j) => sortedIndex(i) < sortedIndex(j))
      (sortedIndex, rank)
    }

    Array.range(0, tuples.dim).map(indexSortAndRank)
  }

  def candidateNearest(centerTupleIndex: Int, cubeSides: Array[HyperCubeSide], step: Int): Array[(TupleIndex, HyperCubeSide)] = {
    cubeSides.flatMap(cubeSide => {
      val (sortedIndex, rank) = sortedData(cubeSide.axis)
      val rankPosition = rank(centerTupleIndex)

      if (cubeSide.direction) {
        val newRank = rankPosition + step
        if (newRank < tuples.length) Some((sortedIndex(newRank), cubeSide)) else None
      } else {
        val newRank = rankPosition - step
        if (0 <= newRank) Some((sortedIndex(newRank), cubeSide)) else None
      }
    })
  }

  def nearest(k: Int, currentTupleIndex: Int): Array[Int] = {


    def axesWhereToTakeNewSteps(epsilon: Double,
                                candidateIndicesAndAxes: Array[(Int, (Int, Boolean))]): Array[(Int, Boolean)] = {
      candidateIndicesAndAxes.flatMap(candidateIndexAndAxis => {
        val (candidateAlongAxisIndex, axis) = candidateIndexAndAxis
        val distanceAlongAxisLowerBound = math.abs(tuples(candidateAlongAxisIndex)(axis._1) - tuples(currentTupleIndex)(axis._1))

        if (epsilon < distanceAlongAxisLowerBound) None else Some(axis)
      })
    }

    @tailrec
    def kNearestIndices(axes: Array[(Int, Boolean)], step: Int,
                        kNearestCandidateIndices: Array[Int], kNearestCandidateDistances: Array[Double]): Array[Int] = {

      println(s"Entering kNearestIndices with")
      println(s"  axes                       = ${TupleArray.print(axes)}")
      println(s"  step                       = $step")
      println(s"  kNearestCandidateIndices   = ${TupleArray.print(kNearestCandidateIndices)}")
      println(s"  kNearestCandidateDistances = ${TupleArray.print(kNearestCandidateDistances)}")

      if (axes.isEmpty) {

        kNearestCandidateIndices

      } else {
        val newCandidateIndices = candidateNearest(currentTupleIndex, axes, step)

        if (newCandidateIndices.isEmpty) {
          kNearestCandidateIndices
        } else {
          println(s"  ==> new harvest of indices = ${TupleArray.print(newCandidateIndices) }")
          val uniqueNewCandidateIndices = (newCandidateIndices.map(_._1).toSet -- kNearestCandidateIndices).toArray

          val candidateIndices = kNearestCandidateIndices ++ uniqueNewCandidateIndices
          val candidateDistances = kNearestCandidateDistances ++
                                   uniqueNewCandidateIndices.map(candidateIndex => distance(tuples(candidateIndex), tuples(currentTupleIndex)))

          val numberOfCandidates = candidateIndices.length
          if (k < numberOfCandidates) {
            val candidateDistancesSortedIndex = Array.range(0, numberOfCandidates).sortWith(candidateDistances(_) < candidateDistances(_))

            val epsilon = candidateDistances(candidateDistancesSortedIndex(k - 1))

            // weed out axes we don't need to look at anymore
            val newAxes = axesWhereToTakeNewSteps(epsilon, newCandidateIndices)

            val kNearestCandidateSortedIndices = candidateDistancesSortedIndex.take(k)
            kNearestIndices(newAxes, step + 1,
                            kNearestCandidateSortedIndices.map(candidateIndices),
                            kNearestCandidateSortedIndices.map(candidateDistances))
          } else {

            kNearestIndices(axes, step + 1,
                            candidateIndices, candidateDistances)
          }
        }
      }
    }


    // initialization
    val axes = Array.range(0, tuples.dim).flatMap(d => Array((d, false), (d, true)))
    kNearestIndices(axes, 1, Array.empty, Array.empty)

  }

}

object Dots {

  def print[T](x: Array[T]): String = x.deep.mkString("Array(", ",", ")")

  //sortedData foreach (tuple => {tuple._1 foreach print; print(" "); tuple._2 foreach print; println})
}
