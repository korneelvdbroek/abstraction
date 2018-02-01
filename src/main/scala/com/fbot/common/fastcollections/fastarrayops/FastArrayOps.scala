package com.fbot.common.fastcollections.fastarrayops

import com.fbot.common.fastcollections.{ArrayIndex, ImmutableArray, ImmutableTupleArray, Tuple}
import com.fbot.common.fastcollections._

import scala.annotation.tailrec
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
trait FastArrayOps {

  type A

  def repr: Array[A]

  def length: Int = repr.length

  def dim: Int = length

  def isEmpty: Boolean = length == 0

  def nonEmpty: Boolean = !isEmpty


  def apply(index: ArrayIndex): A = repr(index.toInt)


  def head: A = repr(0)

  def headOption: Option[A] = if (nonEmpty) Some(head) else None

  def last: A = repr(length - 1)

  def lastOption: Option[A] = if (nonEmpty) Some(last) else None


  def foldLeft[B](z: B)(op: (B, A) => B): B = foldl(0, length, z, op)

  @tailrec
  private def foldl[B](start: Int, end: Int, acc: B, op: (B, A) => B): B = {
    if (start == end) {
      acc
    } else {
      foldl(start + 1, end, op(acc, apply(ArrayIndex(start))), op)
    }
  }

  def foldLeftOrBreakWithIndex[B](z: B)(op: (B, A, ArrayIndex) => (B, Boolean)): B = foldl(0, length, z, break = false, op)

  @tailrec
  private def foldl[B](start: Int, end: Int, acc: B, break: Boolean, op: (B, A, ArrayIndex) => (B, Boolean)): B = {
    if (start == end || break) {
      acc
    } else {
      val (newAcc, newBreak) = op(acc, apply(ArrayIndex(start)), ArrayIndex(start))
      foldl(start + 1, end, newAcc, newBreak, op)
    }
  }

  def forall(p: (A) => Boolean): Boolean = {
    val len = length

    var i = 0
    while (i < len && p(apply(ArrayIndex(i)))) {
      i += 1
    }
    i == len
  }

  def forallWithIndex(p: (A, Int) => Boolean): Boolean = {
    val len = length

    var i = 0
    while (i < len && p(apply(ArrayIndex(i)), i)) {
      i += 1
    }
    i == len
  }

  def count(p: (A) => Boolean): Int = {
    val len = length

    var count = 0
    var i = 0
    while (i < len) {
      if (p(apply(ArrayIndex(i)))) count += 1
      i += 1
    }

    count
  }

  def ++(that: ImmutableArray[A])(implicit evidence: ClassTag[A]): ImmutableArray[A] = {
    val thisLen = repr.length
    val thatLen = that.length

    val concat = new Array[A](thisLen + thatLen)
    System.arraycopy(repr, 0, concat, 0, thisLen)
    System.arraycopy(that.repr, 0, concat, thisLen, thatLen)
    ImmutableArray[A](concat)
  }

  def indexRange: ImmutableArray[Int] = {
    ImmutableArray(Array.range(0, length))
  }

  def map[@specialized(Int, Long, Double) B: ClassTag](f: A => B): ImmutableArray[B] = {
    val len = length
    val res: Array[B] = new Array[B](len)

    var i = 0
    while (i < len) {
      res(i) = f(apply(ArrayIndex(i)))
      i += 1
    }
    ImmutableArray[B](res)
  }

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
          res(d)(i) = f(apply(ArrayIndex(i)))(d)
          d += 1
        }
        i += 1
      }

      ImmutableTupleArray(res.asInstanceOf[Array[ImmutableArray[Double]]])
    }
  }

  def mapWithIndex[@specialized(Int, Long, Double) B: ClassTag](f: (A, ArrayIndex) ⇒ B): ImmutableArray[B] = {
    val len = length
    val res: Array[B] = new Array[B](len)

    var i = 0
    while (i < len) {
      res(i) = f(apply(ArrayIndex(i)), ArrayIndex(i))
      i += 1
    }
    ImmutableArray[B](res)
  }

  private def flattenArray[@specialized(Double, Int, Long) B: ClassTag](unFlattened: Array[ImmutableArray[B]], flattenedLen: Int): ImmutableArray[B] = {
    val len = length
    val res: Array[B] = new Array[B](flattenedLen)

    var i = 0
    var pos = 0
    while (i < len) {
      val element = unFlattened(i)
      System.arraycopy(element, 0, res, pos, element.length)
      pos += element.length
      i += 1
    }
    ImmutableArray[B](res)
  }

  def flatMap(f: A => ImmutableArray[A])(implicit evidence: ClassTag[A]): ImmutableArray[A] = {
    val len = length
    val unFlattened: Array[ImmutableArray[A]] = new Array[ImmutableArray[A]](len)

    var i = 0
    var flattenedLength = 0
    while (i < len) {
      val mapped = f(apply(ArrayIndex(i)))
      unFlattened(i) = mapped
      flattenedLength += mapped.length
      i += 1
    }

    flattenArray(unFlattened, flattenedLength)
  }

  def flatMapWithIndex[B: ClassTag](f: (A, ArrayIndex) => ImmutableArray[B]): ImmutableArray[B] = {
    val len = length
    val unFlattened: Array[ImmutableArray[B]] = new Array[ImmutableArray[B]](len)

    var i = 0
    var flattenedLength = 0
    while (i < len) {
      val mapped = f(apply(ArrayIndex(i)), ArrayIndex(i))
      unFlattened(i) = mapped
      flattenedLength += mapped.length
      i += 1
    }

    flattenArray(unFlattened, flattenedLength)
  }

  def take(k: Int)(implicit evidence: ClassTag[A]): ImmutableArray[A] = {
    val res = new Array[A](k)
    System.arraycopy(repr, 0, res, 0, k)
    ImmutableArray(res)
  }

  def slice(from: Int, until: Int)(implicit evidence: ClassTag[A]): ImmutableArray[A] = {
    val lo = scala.math.max(from, 0)
    val hi = scala.math.min(scala.math.max(until, 0), length)
    val len = scala.math.max(hi - lo, 0)

    val res = new Array[A](len)
    System.arraycopy(repr, lo, res, 0, len)
    ImmutableArray(res)
  }

  def filter(p: A ⇒ Boolean)(implicit evidence: ClassTag[A]): ImmutableArray[A] = {
    val len = length
    val temp: Array[A] = new Array[A](len)

    var i = 0
    var pos = 0
    while (i < len) {
      val x = apply(ArrayIndex(i))
      if (p(x)) {
        temp(pos) = x
        pos += 1
      }
      i += 1
    }

    // resize array
    val res: Array[A] = new Array[A](pos)
    System.arraycopy(temp, 0, res, 0, pos)
    ImmutableArray[A](res)
  }


  def filterByIndex(p: Int => Boolean)(implicit evidence: ClassTag[A]): ImmutableArray[A] = {
    val len = length
    val temp: Array[A] = new Array[A](len)

    var i = 0
    var pos = 0
    while (i < len) {
      if (p(i)) {
        temp(pos) = apply(ArrayIndex(i))
        pos += 1
      }
      i += 1
    }

    // resize array
    val res: Array[A] = new Array[A](pos)
    System.arraycopy(temp, 0, res, 0, pos)
    ImmutableArray[A](res)
  }


  def filterNot(p: A ⇒ Boolean)(implicit evidence: ClassTag[A]): ImmutableArray[A] = {
    filter(!p(_))
  }

  def groupBy[Key: ClassTag](f: A ⇒ Key): Map[Key, ImmutableArray[A]] = {
    repr.groupBy(f).mapValues(grouped => ImmutableArray(grouped))
  }


  // conversions
  def toArray: Array[A] = repr

  def toList: List[A] = genericArrayOps(repr).toList

  def toSet: Set[A] = genericArrayOps(repr).toSet


  // Strings
  override def toString: String = mkString("[", ", ", "]")

  def mkString(start: String, sep: String, end: String): String = {
    genericArrayOps(repr).mkString(start, sep, end)
  }

}
