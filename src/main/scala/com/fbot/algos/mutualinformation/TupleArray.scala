package com.fbot.algos.mutualinformation

import TupleOps._


import scala.annotation.tailrec

/**
  * Copyright (C) 5/30/2017 - REstore NV
  *
  */
case class TupleArray(tuples: Array[Tuple]) {

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



case class TupleCloud(tupleCloud: TupleArray) {

  val sortedData: Array[(Array[TupleIndex], Array[Int])] = {
    def indexSortAndRank(axis: Int): (Array[TupleIndex], Array[Int]) = {
      val sortedIndex = tupleCloud.indexRange.sortWith(tupleCloud(_)(axis) < tupleCloud(_)(axis))
      val rank = Array.range(0, tupleCloud.length).sortWith((i, j) => sortedIndex(i) < sortedIndex(j))
      (sortedIndex, rank)
    }

    Array.range(0, tupleCloud.dim).map(indexSortAndRank)
  }

  def candidateNearest(centerTupleIndex: TupleIndex, cubeSides: Array[HyperCubeSide], step: Int): Array[(TupleIndex, HyperCubeSide)] = {
    cubeSides.flatMap(cubeSide => {
      val (sortedIndex, rank) = sortedData(cubeSide.axis)
      val rankPosition = rank(centerTupleIndex.i)

      if (cubeSide.direction) {
        val newRank = rankPosition + step
        if (newRank < tupleCloud.length) Some((sortedIndex(newRank), cubeSide)) else None
      } else {
        val newRank = rankPosition - step
        if (0 <= newRank) Some((sortedIndex(newRank), cubeSide)) else None
      }
    })
  }

  def nearest(k: Int, currentTupleIndex: TupleIndex): Array[TupleIndex] = {

    def cubeSidesToGrow(epsilon: Double,
                        tuplesOnCubeSide: Array[(TupleIndex, HyperCubeSide)]): Array[HyperCubeSide] = {
      val cubeSidesToGrow = tuplesOnCubeSide.flatMap(tupleOnCubeSide => {
        val (tupleOnCubeSideIndex, cubeSide) = tupleOnCubeSide
        val distanceToCubeSide = math.abs(tupleCloud(tupleOnCubeSideIndex)(cubeSide.axis) - tupleCloud(currentTupleIndex)(cubeSide.axis))

        if (epsilon < distanceToCubeSide) None else Some(cubeSide)
      })

      // check if another axis already swept out the entire hypercube, since then we are done
      if (cubeSidesToGrow.map(_.axis).distinct.length < tupleCloud.dim) {
        Array.empty
      } else {
        cubeSidesToGrow
      }

    }

    @tailrec
    def kNearestIndices(axes: Array[HyperCubeSide], step: Int,
                        kNearestCandidateIndices: Array[TupleIndex], kNearestCandidateDistances: Array[Double]): Array[TupleIndex] = {

//      println(s"Entering kNearestIndices with")
      println(s"  axes                       = ${TupleCloud.print(axes) }")
      println(s"  step                       = $step")
//      println(s"  kNearestCandidateIndices   = ${TupleCloud.print(kNearestCandidateIndices) }")
//      println(s"  kNearestCandidateDistances = ${TupleCloud.print(kNearestCandidateDistances) }")

      if (axes.isEmpty) {

        kNearestCandidateIndices

      } else {
        val newCandidateIndices = candidateNearest(currentTupleIndex, axes, step)

        if (newCandidateIndices.isEmpty) {
          kNearestCandidateIndices
        } else {
//          println(s"  ==> new harvest of indices = ${TupleCloud.print(newCandidateIndices) }")
          val uniqueNewCandidateIndices = (newCandidateIndices.map(_._1).toSet -- kNearestCandidateIndices).toArray

          val candidateIndices = kNearestCandidateIndices ++ uniqueNewCandidateIndices
          val candidateDistances = kNearestCandidateDistances ++
                                   uniqueNewCandidateIndices.map(candidateIndex => distance(tupleCloud(candidateIndex), tupleCloud(currentTupleIndex)))

          val numberOfCandidates = candidateIndices.length
          if (k < numberOfCandidates) {
            val candidateDistancesSortedIndex = Array.range(0, numberOfCandidates).sortWith(candidateDistances(_) < candidateDistances(_))

            val epsilon = candidateDistances(candidateDistancesSortedIndex(k - 1))

            // weed out axes we don't need to look at anymore
            val newAxes = cubeSidesToGrow(epsilon, newCandidateIndices)

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
    val axes = Array.range(0, tupleCloud.dim).flatMap(d => Array(HyperCubeSide(d, direction = false), HyperCubeSide(d, direction = true)))
    kNearestIndices(axes, 1, Array.empty, Array.empty)

  }


  def nearestBruteForce(k: Int, currentTupleIndex: TupleIndex): (Int, Array[TupleIndex]) = {
    val currentTuple = tupleCloud(currentTupleIndex)
    val otherTuplesSortedByDistance = tupleCloud.indexRange
      .filterNot(_ == currentTupleIndex)
      .map(index => (index, distance(tupleCloud(index), currentTuple)))
      .sortBy(_._2)


    var i = k
    while (i < otherTuplesSortedByDistance.length && otherTuplesSortedByDistance(k - 1)._2 ==  otherTuplesSortedByDistance(i)._2) {
      i += 1
    }

    (i, otherTuplesSortedByDistance.take(k).map(_._1))
  }

}

object TupleCloud {

  def apply(data: Tuple*): TupleCloud = TupleCloud(TupleArray(data.toArray))

  def apply(data: Array[Tuple]): TupleCloud = TupleCloud(TupleArray(data))

  def print[T](x: Array[T]): String = x.deep.mkString("Array(", ",", ")")

}
