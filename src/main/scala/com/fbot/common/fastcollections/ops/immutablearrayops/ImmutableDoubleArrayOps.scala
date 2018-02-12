package com.fbot.common.fastcollections.ops.immutablearrayops

import com.fbot.common.fastcollections.core.LiteWrappedArray
import com.fbot.common.fastcollections.{ArrayIndex, ImmutableArray, Tuple, _}

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
case class ImmutableDoubleArrayOps(repr: LiteWrappedArray[Double]) extends AnyVal with ImmutableArrayOps {

  // publish methods from LiteWrappedArray
  def length: Int = repr.length

  def isEmpty: Boolean = repr.isEmpty

  def nonEmpty: Boolean = repr.nonEmpty

  def apply(index: ArrayIndex): Double = repr.apply(index.toInt)

  def head: Double = repr.head

  def headOption: Option[Double] = repr.headOption

  def last: Double = repr.last

  def lastOption: Option[Double] = repr.lastOption

  def foldLeft[B](z: B)(op: (B, Double) => B): B = repr.foldLeft(z)(op)

  def foldLeftOrBreakWithIndex[B](z: B)(op: (B, Double, ArrayIndex) => (B, Boolean)): B = {
    repr.foldLeftOrBreakWithIndex(z)((acc, elem, index) => op(acc, elem, ArrayIndex(index)))
  }

  def forall(p: (Double) => Boolean): Boolean = repr.forall(p)

  def forallWithIndex(p: (Double, ArrayIndex) => Boolean): Boolean = {
    repr.forallWithIndex((elem, index) => p(elem, ArrayIndex(index)))
  }

  def count(p: (Double) => Boolean): Int = repr.count(p)

  def ++ (that: ImmutableArray[Double]): ImmutableArray[Double] = ImmutableArray(repr ++ that.repr)

  def slice(from: Int, until: Int): ImmutableArray[Double] = ImmutableArray(repr.slice(from, until))

  def toArray: Array[Double] = repr.array

  def toList: List[Double] = repr.toList

  def toSet: Set[Double] = repr.toSet


  // mapping methods
  def indexRange: ImmutableArray[Int] = ImmutableArray(repr.indexRange)

  def map[@specialized(Int, Long, Double) B: ClassTag](f: Double => B): ImmutableArray[B] = ImmutableArray(repr.map(f))

  def mapWithIndex[@specialized(Int, Long, Double) B: ClassTag](f: (Double, ArrayIndex) ⇒ B): ImmutableArray[B] = {
    ImmutableArray(repr.mapWithIndex((elem, index) => f(elem, ArrayIndex(index))))
  }

  def flatMap(f: Double => ImmutableArray[Double])(implicit evidence: ClassTag[Double]): ImmutableArray[Double] = {
    ImmutableArray(repr.flatMap(
      elem => f(elem).repr))
  }

  def flatMapWithIndex[B: ClassTag](f: (Double, ArrayIndex) => ImmutableArray[B]): ImmutableArray[B] = {
    ImmutableArray(repr.flatMapWithIndex((elem, index) => f(elem, ArrayIndex(index)).repr))
  }

  def take(k: Int)(implicit evidence: ClassTag[Double]): ImmutableArray[Double] = ImmutableArray(repr.take(k))

  def filter(p: Double ⇒ Boolean)(implicit evidence: ClassTag[Double]): ImmutableArray[Double] = ImmutableArray(repr.filter(p))

  def filterByIndex(p: ArrayIndex => Boolean)(implicit evidence: ClassTag[Double]): ImmutableArray[Double] = {
    ImmutableArray(repr.filterByIndex(
      index => p(ArrayIndex(index))))
  }

  def filterNot(p: Double ⇒ Boolean)(implicit evidence: ClassTag[Double]): ImmutableArray[Double] = ImmutableArray(repr.filterNot(p))

  def groupBy[Key: ClassTag](f: Double ⇒ Key): Map[Key, ImmutableArray[Double]] = repr.groupBy(f).mapValues(values => ImmutableArray(values))


  // new methods
  def mkString: String = repr.mkString("[", ", ", "]")

  def mapToTuple(f: Double => Tuple): ImmutableTupleArray = {
    val len = length

    if (len == 0) {
      ImmutableTupleArray.empty(0)
    } else {
      val dim = f(head).dim

      // allocate
      var d: Int = 0
      val res: Array[Array[Double]] = new Array[Array[Double]](dim)
      while (d < dim) {
        res(d) = new Array[Double](len)
        d += 1
      }

      // fill with data
      var i = 0
      while (i < len) {
        var d = 0
        while (d < dim) {
          res(d)(i) = f(repr.array(i))(d)
          d += 1
        }
        i += 1
      }

      ImmutableTupleArray(res.map(arr => new LiteWrappedArray(arr)).asInstanceOf[Array[ImmutableArray[Double]]])
    }
  }


  // new methods
  def indexOfSorted: ImmutableArray[ArrayIndex] = {
    val len = length

    if (len <= 1) {
      ImmutableArray(repr.array.map(_ => ArrayIndex(0)))
    } else {
      val indices: Array[AnyRef] = Array.range(0, len).map(_.asInstanceOf[AnyRef])
      val ord: Ordering[Int] = Ordering.fromLessThan((i, j) => repr.array(i) < repr.array(j))
      java.util.Arrays.sort(indices, ord.asInstanceOf[Ordering[Object]])
      ImmutableArray(indices.map(_.asInstanceOf[ArrayIndex]))
    }
  }

  def partialSort(k: Int): (ImmutableArray[ArrayIndex], ImmutableArray[Double]) = {
    val len = length
    val pq: mutable.PriorityQueue[(ArrayIndex, Double)] = new mutable.PriorityQueue[(ArrayIndex, Double)]()(Ordering.by(_._2))

    // load up the PQ
    var i = 0
    while (i < k && i < len) {
      pq.enqueue((ArrayIndex(i), repr.array(i)))
      i += 1
    }

    // evaluate rest of array
    while (i < len) {
      if (repr.array(i) <= pq.head._2) {
        pq.dequeue()
        pq.enqueue((ArrayIndex(i), repr.array(i)))
      }
      i += 1
    }

    val (sortedIndices, sortedValues) = pq.dequeueAll.reverse.unzip
    (ImmutableArray(sortedIndices), ImmutableArray(sortedValues))
  }


  // conversion
  def toTuple: Tuple = Tuple(repr.array)

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
      res(i) = f(apply(ArrayIndex(i)), rhs.repr.array(i))
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
