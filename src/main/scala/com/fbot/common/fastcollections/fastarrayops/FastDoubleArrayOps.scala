package com.fbot.common.fastcollections.fastarrayops

import com.fbot.common.fastcollections.{ArrayIndex, ImmutableArray, Tuple}
import com.fbot.common.fastcollections._

import scala.collection.mutable
import scala.math.Ordering
import scala.reflect.ClassTag

/**
  * Copyright (C) 2017-2018  korneelvdbroek
  *
  * This program is free software: you can redistribute it and/or modify
  * it under the terms of the GNU General Public License as published by
  * the Free Software Foundation, either version 3 of the License, or
  * (at your option) any later version.
  *
  * This program is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU General Public License for more details.
  *
  * You should have received a copy of the GNU General Public License
  * along with this program.  If not, see <http://www.gnu.org/licenses/>.
  *
  */
case class FastDoubleArrayOps(repr: Array[Double]) extends AnyVal with FastArrayOps {

  type A = Double

  def indexOfSorted: ImmutableArray[ArrayIndex] = {
    val len = length

    if (len <= 1) {
      ImmutableArray(repr.map(_ => ArrayIndex(0)))
    } else {
      val indices: Array[AnyRef] = Array.range(0, len).map(_.asInstanceOf[AnyRef])
      val ord: Ordering[Int] = Ordering.fromLessThan((i, j) => repr(i) < repr(j))
      java.util.Arrays.sort(indices, ord.asInstanceOf[Ordering[Object]])
      ImmutableArray(indices.asInstanceOf[Array[ArrayIndex]])
    }
  }

  def partialSort(k: Int): (ImmutableArray[ArrayIndex], ImmutableArray[Double]) = {
    val len = length
    val pq: mutable.PriorityQueue[(ArrayIndex, Double)] = new mutable.PriorityQueue[(ArrayIndex, Double)]()(Ordering.by(_._2))

    // load up the PQ
    var i = 0
    while (i < k && i < len) {
      pq.enqueue((ArrayIndex(i), repr(i)))
      i += 1
    }

    // evaluate rest of array
    while (i < len) {
      if (repr(i) <= pq.head._2) {
        pq.dequeue()
        pq.enqueue((ArrayIndex(i), repr(i)))
      }
      i += 1
    }

    val (sortedIndices, sortedValues) = pq.dequeueAll.reverse.unzip
    (ImmutableArray(sortedIndices), ImmutableArray(sortedValues))
  }


  // conversion
  def toTuple: Tuple = Tuple(repr)

  // Arithmetic
  def + (rhs: ImmutableArray[Double]): ImmutableArray[Double] = {
    elementWise(rhs, _ + _)
  }

  def - (rhs: ImmutableArray[Double]): ImmutableArray[Double] = {
    elementWise(rhs, _ - _)
  }

  def * (rhs: ImmutableArray[Double]): ImmutableArray[Double] = {
    elementWise(rhs, _ * _)
  }

  def / (rhs: ImmutableArray[Double]): ImmutableArray[Double] = {
    elementWise(rhs, _ / _)
  }

  @inline
  private def elementWise(rhs: ImmutableArray[Double], f: (Double, Double) => Double): ImmutableArray[Double] = {
    val len = length
    val res: Array[Double] = new Array[Double](len)

    var i = 0
    while (i < len) {
      res(i) = f(apply(ArrayIndex(i)), rhs.repr.apply(i))
      i += 1
    }
    ImmutableArray[Double](res)
  }

  def unary_-(): ImmutableArray[Double] = {
    map((x: Double) => -x)
  }

  def sum: Double = {
    val len = length

    var i = 0
    var sum: Double = 0d
    while (i < len) {
      sum += apply(ArrayIndex(i))
      i += 1
    }
    sum
  }

  def max: Double = {
    val len = length

    var i = 0
    var max: Double = Double.NegativeInfinity
    while (i < len) {
      if (max < apply(ArrayIndex(i))) max = apply(ArrayIndex(i))
      i += 1
    }
    max
  }
}
