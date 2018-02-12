package com.fbot.common.fastcollections.ops.immutablearrayops

import com.fbot.common.fastcollections.{ArrayIndex, ImmutableArray}
import com.fbot.common.fastcollections._
import com.fbot.common.fastcollections.core.LiteWrappedArray

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
case class ImmutableLongArrayOps(repr: LiteWrappedArray[Long]) extends AnyVal with ImmutableArrayOps  {

  // publish methods from LiteWrappedArray
  def length: Int = repr.length

  def isEmpty: Boolean = repr.isEmpty

  def nonEmpty: Boolean = repr.nonEmpty

  def apply(index: ArrayIndex): Long = repr.apply(index.toInt)

  def head: Long = repr.head

  def headOption: Option[Long] = repr.headOption

  def last: Long = repr.last

  def lastOption: Option[Long] = repr.lastOption

  def foldLeft[B](z: B)(op: (B, Long) => B): B = repr.foldLeft(z)(op)

  def foldLeftOrBreakWithIndex[B](z: B)(op: (B, Long, ArrayIndex) => (B, Boolean)): B = {
    repr.foldLeftOrBreakWithIndex(z)((acc, elem, index) => op(acc, elem, ArrayIndex(index)))
  }

  def forall(p: (Long) => Boolean): Boolean = repr.forall(p)

  def forallWithIndex(p: (Long, ArrayIndex) => Boolean): Boolean = {
    repr.forallWithIndex((elem, index) => p(elem, ArrayIndex(index)))
  }

  def count(p: (Long) => Boolean): Int = repr.count(p)

  def ++ (that: ImmutableArray[Long]): ImmutableArray[Long] = ImmutableArray(repr ++ that.repr)

  def slice(from: Int, until: Int): ImmutableArray[Long] = ImmutableArray(repr.slice(from, until))

  def toArray: Array[Long] = repr.array

  def toList: List[Long] = repr.toList

  def toSet: Set[Long] = repr.toSet



  // mapping methods
  def indexRange: ImmutableArray[Int] = ImmutableArray(repr.indexRange)

  def map[@specialized(Int, Long, Double) B: ClassTag](f: Long => B): ImmutableArray[B] = ImmutableArray(repr.map(f))

  def mapWithIndex[@specialized(Int, Long, Double) B: ClassTag](f: (Long, ArrayIndex) ⇒ B): ImmutableArray[B] = {
    ImmutableArray(repr.mapWithIndex((elem, index) => f(elem, ArrayIndex(index))))
  }

  def flatMap(f: Long => ImmutableArray[Long])(implicit evidence: ClassTag[Long]): ImmutableArray[Long] = {
    ImmutableArray(repr.flatMap(elem => f(elem).repr))
  }

  def flatMapWithIndex[B: ClassTag](f: (Long, ArrayIndex) => ImmutableArray[B]): ImmutableArray[B] = {
    ImmutableArray(repr.flatMapWithIndex((elem, index) => f(elem, ArrayIndex(index)).repr))
  }

  def take(k: Int)(implicit evidence: ClassTag[Long]): ImmutableArray[Long] = ImmutableArray(repr.take(k))

  def filter(p: Long ⇒ Boolean)(implicit evidence: ClassTag[Long]): ImmutableArray[Long] = ImmutableArray(repr.filter(p))

  def filterByIndex(p: ArrayIndex => Boolean)(implicit evidence: ClassTag[Long]): ImmutableArray[Long] = {
    ImmutableArray(repr.filterByIndex(index => p(ArrayIndex(index))))
  }

  def filterNot(p: Long ⇒ Boolean)(implicit evidence: ClassTag[Long]): ImmutableArray[Long] = ImmutableArray(repr.filterNot(p))

  def groupBy[Key: ClassTag](f: Long ⇒ Key): Map[Key, ImmutableArray[Long]] = repr.groupBy(f).mapValues(values => ImmutableArray(values))



  // new methods
  def mkString: String = repr.mkString("[", ", ", "]")

  def mapToTuple(f: Long => Tuple): ImmutableTupleArray = {
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

  def sorted: ImmutableArray[Long] = {
    val len = length

    if (len <= 1) {
      ImmutableArray(repr.array)
    } else {
      // we make a copy, such that the unsorted ImmutableArray remains untouched
      val res: Array[Long] = new Array[Long](len)
      System.arraycopy(repr.array, 0, res, 0, len)
      java.util.Arrays.sort(res)
      ImmutableArray(res)
    }

  }

  def partialSort(k: Long): (ImmutableArray[ArrayIndex], ImmutableArray[Long]) = {
    val len = length
    val pq: mutable.PriorityQueue[(ArrayIndex, Long)] = new mutable.PriorityQueue[(ArrayIndex, Long)]()(Ordering.by(_._2))

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


  // Arithmetic
  def + (rhs: ImmutableArray[Long]): ImmutableArray[Long] = {
    elementWise(rhs, _ + _)
  }

  def - (rhs: ImmutableArray[Long]): ImmutableArray[Long] = {
    elementWise(rhs, _ - _)
  }

  def * (rhs: ImmutableArray[Long]): ImmutableArray[Long] = {
    elementWise(rhs, _ * _)
  }

  @inline
  private def elementWise(rhs: ImmutableArray[Long], f: (Long, Long) => Long): ImmutableArray[Long] = {
    val len = length
    val res: Array[Long] = new Array[Long](len)

    var i = 0
    while (i < len) {
      res(i) = f(apply(ArrayIndex(i)), rhs.repr.array.apply(i))
      i += 1
    }
    ImmutableArray[Long](res)
  }

  def unary_-(): ImmutableArray[Long] = {
    map((x: Long) => -x)
  }

  def sum: Long = {
    val len = length

    var i = 0
    var sum: Long = 0L
    while (i < len) {
      sum += apply(ArrayIndex(i))
      i += 1
    }
    sum
  }

  def max: Long = {
    val len = length

    var i = 0
    var max = Long.MinValue
    while (i < len) {
      if (max < apply(ArrayIndex(i))) max = apply(ArrayIndex(i))
      i += 1
    }
    max
  }

}
