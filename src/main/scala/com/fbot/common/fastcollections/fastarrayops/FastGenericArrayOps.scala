package com.fbot.common.fastcollections.fastarrayops

import com.fbot.common.fastcollections.{ArrayIndex, ImmutableArray, ImmutableTupleArray, LiteWrappedArray, Tuple, _}

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
case class FastGenericArrayOps[A](repr: LiteWrappedArray[A]) extends AnyVal with FastArrayOps {

  // publish methods from LiteWrappedArray
  def length: Int = repr.length

  def isEmpty: Boolean = repr.isEmpty

  def nonEmpty: Boolean = repr.nonEmpty

  def apply(index: ArrayIndex): A = repr.apply(index.toInt)

  def head: A = repr.head

  def headOption: Option[A] = repr.headOption

  def last: A = repr.last

  def lastOption: Option[A] = repr.lastOption

  def foldLeft[B](z: B)(op: (B, A) => B): B = repr.foldLeft(z)(op)

  def foldLeftOrBreakWithIndex[B](z: B)(op: (B, A, ArrayIndex) => (B, Boolean)): B = {
    repr.foldLeftOrBreakWithIndex(z)((acc, elem, index) => op(acc, elem, ArrayIndex(index)))
  }

  def forall(p: (A) => Boolean): Boolean = repr.forall(p)

  def forallWithIndex(p: (A, ArrayIndex) => Boolean): Boolean = {
    repr.forallWithIndex((elem, index) => p(elem, ArrayIndex(index)))
  }

  def count(p: (A) => Boolean): Int = repr.count(p)

  def ++ (that: ImmutableArray[A])(implicit evidence: ClassTag[A]): ImmutableArray[A] = ImmutableArray(repr ++ that.repr)

  def slice(from: Int, until: Int)(implicit evidence: ClassTag[A]): ImmutableArray[A] = ImmutableArray(repr.slice(from, until))

  def toArray: Array[A] = repr.array

  def toList: List[A] = repr.toList

  def toSet: Set[A] = repr.toSet


  // mapping methods
  def indexRange: ImmutableArray[Int] = ImmutableArray(repr.indexRange)

  def map[@specialized(Int, Long, Double) B: ClassTag](f: A => B): ImmutableArray[B] = ImmutableArray(repr.map(f))

  def mapWithIndex[@specialized(Int, Long, Double) B: ClassTag](f: (A, ArrayIndex) ⇒ B): ImmutableArray[B] = ImmutableArray(repr
                                                                                                                              .mapWithIndex((elem,
                                                                                                                                             index) => f
                                                                                                                              (elem, ArrayIndex(index))))

  def flatMap(f: A => ImmutableArray[A])(implicit evidence: ClassTag[A]): ImmutableArray[A] = ImmutableArray(repr.flatMap(elem => f(elem).repr))

  def flatMapWithIndex[B: ClassTag](f: (A, ArrayIndex) => ImmutableArray[B]): ImmutableArray[B] = {
    ImmutableArray(repr.flatMapWithIndex((elem, index) => f(elem, ArrayIndex(index)).repr))
  }

  def take(k: Int)(implicit evidence: ClassTag[A]): ImmutableArray[A] = ImmutableArray(repr.take(k))

  def filter(p: A ⇒ Boolean)(implicit evidence: ClassTag[A]): ImmutableArray[A] = ImmutableArray(repr.filter(p))

  def filterByIndex(p: ArrayIndex => Boolean)(implicit evidence: ClassTag[A]): ImmutableArray[A] = ImmutableArray(repr
                                                                                                                    .filterByIndex(
                                                                                                                      index => p(ArrayIndex(index))))

  def filterNot(p: A ⇒ Boolean)(implicit evidence: ClassTag[A]): ImmutableArray[A] = ImmutableArray(repr.filterNot(p))

  def groupBy[Key: ClassTag](f: A ⇒ Key): Map[Key, ImmutableArray[A]] = repr.groupBy(f).mapValues(values => ImmutableArray(values))


  // new methods
  def mkString: String = repr.mkString("[", ", ", "]")

  def mapToTuple(f: A => Tuple): ImmutableTupleArray = {
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

}
