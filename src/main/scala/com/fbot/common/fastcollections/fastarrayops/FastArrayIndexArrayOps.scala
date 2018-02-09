package com.fbot.common.fastcollections.fastarrayops

import com.fbot.common.fastcollections.{ArrayIndex, ImmutableArray, ImmutableTupleArray, LiteWrappedArray, Tuple}
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
case class FastArrayIndexArrayOps(repr: LiteWrappedArray[Int]) extends AnyVal with FastArrayOps {

  // publish methods from LiteWrappedArray
  def length: Int = repr.length

  def isEmpty: Boolean = repr.isEmpty

  def nonEmpty: Boolean = repr.nonEmpty

  def apply(index: ArrayIndex): ArrayIndex = ArrayIndex(repr.apply(index.toInt))

  def head: ArrayIndex = ArrayIndex(repr.head)

  def headOption: Option[ArrayIndex] = ArrayIndex.subst(repr.headOption)

  def last: ArrayIndex = ArrayIndex(repr.last)

  def lastOption: Option[ArrayIndex] = ArrayIndex.subst(repr.lastOption)

  def foldLeft[B](z: B)(op: (B, ArrayIndex) => B): B = repr.foldLeft(z)((acc, elem) => op(acc, ArrayIndex(elem)))

  def foldLeftOrBreakWithIndex[B](z: B)(op: (B, ArrayIndex, ArrayIndex) => (B, Boolean)): B = {
    repr.foldLeftOrBreakWithIndex(z)((acc, elem, index) => op(acc, ArrayIndex(elem), ArrayIndex(index)))
  }

  def forall(p: (ArrayIndex) => Boolean): Boolean = repr.forall(elem => p(ArrayIndex(elem)))

  def forallWithIndex(p: (ArrayIndex, ArrayIndex) => Boolean): Boolean = {
    repr.forallWithIndex((elem, index) => p(ArrayIndex(elem), ArrayIndex(index)))
  }

  def count(p: (ArrayIndex) => Boolean): Int = repr.count(elem => p(ArrayIndex(elem)))

  def ++ (that: ImmutableArray[ArrayIndex]): ImmutableArray[ArrayIndex] = ArrayIndex.subst(ImmutableArray(repr ++ that.repr))

  def slice(from: Int, until: Int): ImmutableArray[ArrayIndex] = ArrayIndex.subst(ImmutableArray(repr.slice(from, until)))

  def toArray: Array[ArrayIndex] = ArrayIndex.subst(repr.array)

  def toList: List[ArrayIndex] = ArrayIndex.subst(repr.toList)

  def toSet: Set[ArrayIndex] = ArrayIndex.subst(repr.toSet)



  // mapping methods
  def indexRange: ImmutableArray[ArrayIndex] = ArrayIndex.subst(ImmutableArray(repr.indexRange))

  def map[@specialized(Int, Long, Double) B: ClassTag](f: ArrayIndex => B): ImmutableArray[B] = ImmutableArray(repr.map(elem => f(ArrayIndex(elem))))

  def mapWithIndex[@specialized(Int, Long, Double) B: ClassTag](f: (ArrayIndex, ArrayIndex) ⇒ B): ImmutableArray[B] = {
    ImmutableArray(repr.mapWithIndex((elem, index) => f(ArrayIndex(elem), ArrayIndex(index))))
  }

  def flatMap(f: ArrayIndex => ImmutableArray[ArrayIndex])(implicit evidence: ClassTag[ArrayIndex]): ImmutableArray[ArrayIndex] = {
    ArrayIndex.subst(ImmutableArray(repr.flatMap(elem => f(ArrayIndex(elem)).repr)))
  }

  def flatMapWithIndex[B: ClassTag](f: (ArrayIndex, ArrayIndex) => ImmutableArray[B]): ImmutableArray[B] = {
    ImmutableArray(repr.flatMapWithIndex((elem, index) => f(ArrayIndex(elem), ArrayIndex(index)).repr))
  }

  def take(k: Int)(implicit evidence: ClassTag[ArrayIndex]): ImmutableArray[ArrayIndex] = ArrayIndex.subst(ImmutableArray(repr.take(k)))

  def filter(p: ArrayIndex ⇒ Boolean)(implicit evidence: ClassTag[ArrayIndex]): ImmutableArray[ArrayIndex] = {
    ArrayIndex.subst(ImmutableArray(repr.filter(elem => p(ArrayIndex(elem)))))
  }

  def filterByIndex(p: ArrayIndex => Boolean)(implicit evidence: ClassTag[ArrayIndex]): ImmutableArray[ArrayIndex] = {
    ArrayIndex.subst(ImmutableArray(repr.filterByIndex(index => p(ArrayIndex(index)))))
  }

  def filterNot(p: ArrayIndex ⇒ Boolean)(implicit evidence: ClassTag[ArrayIndex]): ImmutableArray[ArrayIndex] = {
    ArrayIndex.subst(ImmutableArray(repr.filterNot(elem => p(ArrayIndex(elem)))))
  }

  def groupBy[Key: ClassTag](f: ArrayIndex ⇒ Key): Map[Key, ImmutableArray[ArrayIndex]] = {
    repr.groupBy(elem => f(ArrayIndex(elem))).mapValues(values => ArrayIndex.subst(ImmutableArray(values)))
  }



  // new methods
  def mkString: String = repr.mkString("[", ", ", "]")

  def mapToTuple(f: ArrayIndex => Tuple): ImmutableTupleArray = {
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
          res(d)(i) = f(ArrayIndex(repr.array(i)))(d)
          d += 1
        }
        i += 1
      }

      ImmutableTupleArray(res.map(arr => new LiteWrappedArray(arr)).asInstanceOf[Array[ImmutableArray[Double]]])
    }
  }

}
