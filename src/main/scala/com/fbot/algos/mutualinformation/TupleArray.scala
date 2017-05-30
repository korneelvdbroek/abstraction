package com.fbot.algos.mutualinformation

import TupleOps._

/**
  * Copyright (C) 5/30/2017 - REstore NV
  *
  */
case class TupleArray(tuples: Array[Tuple]) {

  lazy val sortedData: Array[(Array[Int], Array[Int])] = {
    def indexSort(axis: Int): (Array[Int], Array[Int]) = {
      val index = Array.range(0, length)
      val argsort = index.sortWith(tuples(_)(axis) < tuples(_)(axis))
      val rank = index.sortWith(argsort(_) < argsort(_))
      (argsort, rank)
    }

    Array.range(0, dim).map(indexSort)
  }

  def length: Int = tuples.length

  def dim: Int = {
    if (tuples.length > 0) tuples(0).dim else 0
  }


  override def toString: String = {
    tuples.mkString("[", ",\n", "]")
  }

  def nextTupleIndex(i: Int, axis: Int): Option[Int] = {
    val (sortedIndex, rank) = sortedData(axis)
    val sortedPosition = rank(i)
    if (sortedPosition < length - 1) {
      Some(sortedIndex(sortedPosition + 1))
    } else {
      None
    }
  }

  def previousTupleIndex(i: Int, axis: Int): Option[Int] = {
    val (sortedIndex, rank) = sortedData(axis)
    val sortedPosition = rank(i)
    if (sortedPosition > 0) {
      Some(sortedIndex(sortedPosition - 1))
    } else {
      None
    }
  }

  def nearest(i: Int): Tuple = {
    //sortedData foreach (tuple => {tuple._1 foreach print; print(" "); tuple._2 foreach print; println})

    val x: Tuple = tuples(i)

    // continue here: find minimum (change into foldLeft)
    Array.range(0, dim).map(axes => {
      println(previousTupleIndex(i, axes).map(previousIndex => distance(tuples(previousIndex), x)))
      println(nextTupleIndex(i, axes).map(nextIndex => distance(tuples(nextIndex), x)))
    })
    Tuple(0)
  }

}

object TupleArray {

  def apply(data: Tuple*): TupleArray = new TupleArray(data.toArray)

}
