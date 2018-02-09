package com.fbot.common.fastcollections.tupleops

import com.fbot.common.fastcollections.Tuple
import com.fbot.common.fastcollections._

import scala.annotation.tailrec

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
// This Value Class published the methods of LiteWrappedArray which are available to Tuple (no boxing, given that it's a value class)
case class TupleOps(repr: LiteWrappedArray[Double]) extends AnyVal {

  // publish methods from LiteWrappedArray
  def length: Int = repr.length

  def isEmpty: Boolean = repr.isEmpty

  def nonEmpty: Boolean = repr.nonEmpty

  def apply(index: Int): Double = repr.apply(ArrayIndex(index))

  def head: Double = repr.head

  def headOption: Option[Double] = repr.headOption

  def last: Double = repr.last

  def lastOption: Option[Double] = repr.lastOption

  def foldLeft[B](z: B)(op: (B, Double) => B): B = repr.foldLeft(z)(op)

  def foldLeftOrBreakWithIndex[B](z: B)(op: (B, Double, Int) => (B, Boolean)): B = repr.foldLeftOrBreakWithIndex(z)(op)

  def forall(p: (Double) => Boolean): Boolean = repr.forall(p)

  def forallWithIndex(p: (Double, Int) => Boolean): Boolean = repr.forallWithIndex(p)

  def count(p: (Double) => Boolean): Int = repr.count(p)

  def ++ (that: Tuple): Tuple = Tuple(repr ++ that.repr)

  def slice(from: Int, until: Int): Tuple = Tuple(repr.slice(from, until))

  def toArray: Array[Double] = repr.array

  def toList: List[Double] = repr.toList

  def toSet: Set[Double] = repr.toSet

  def mkString(start: String, sep: String, end: String): String = repr.mkString(start, sep, end)



  // new methods
  def dim: Int = length

  def mkString: String = mkString("(", ", ", ")")

  def + (rhs: Tuple): Tuple = {
    elementWise(_ + _)(rhs)
  }

  def - (rhs: Tuple): Tuple = {
    elementWise(_ - _)(rhs)
  }

  def * (rhs: Tuple): Tuple = {
    elementWise(_ * _)(rhs)
  }

  def / (rhs: Tuple): Tuple = {
    elementWise(_ / _)(rhs)
  }

  @inline
  private def elementWise(f: (Double, Double) => Double)(rhs: Tuple): Tuple = {
    val len = length
    val res: Array[Double] = new Array[Double](len)

    var i = 0
    while (i < len) {
      res(i) = f(apply(i), rhs.repr.array(i))
      i += 1
    }
    Tuple(res)
  }

  def unary_-(): Tuple = {
    map(-_)
  }

  @inline
  private def map(f: Double => Double): Tuple = {
    val len = length
    val res: Array[Double] = new Array[Double](length)

    var i = 0
    while (i < len) {
      res(i) = f(apply(i))
      i += 1
    }
    Tuple(res)
  }


}
