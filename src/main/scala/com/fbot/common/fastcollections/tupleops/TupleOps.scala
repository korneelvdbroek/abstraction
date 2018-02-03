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
case class TupleOps(repr: Array[Double]) extends AnyVal {

  def length: Int = repr.length

  def dim: Int = length

  def isEmpty: Boolean = length == 0

  def nonEmpty: Boolean = !isEmpty


  def apply(index: Int): Double = repr(index)


  def head: Double = repr(0)

  def headOption: Option[Double] = if (nonEmpty) Some(head) else None

  def last: Double = repr(length - 1)

  def lastOption: Option[Double] = if (nonEmpty) Some(last) else None


  def foldLeft[B](z: B)(op: (B, Double) => B): B = foldl(0, length, z, op)

  @tailrec
  private def foldl[B](start: Int, end: Int, acc: B, op: (B, Double) => B): B = {
    if (start == end) {
      acc
    } else {
      foldl(start + 1, end, op(acc, apply(start)), op)
    }
  }

  def foldLeftOrBreak[B](z: B)(op: (B, Double) => (B, Boolean)): B = foldl(0, length, z, break = false, op)

  @tailrec
  private def foldl[B](start: Int, end: Int, acc: B, break: Boolean, op: (B, Double) => (B, Boolean)): B = {
    if (start == end || break) {
      acc
    } else {
      val (newAcc, newBreak) = op(acc, apply(start))
      foldl(start + 1, end, newAcc, newBreak, op)
    }
  }

  def forall(p: (Double) => Boolean): Boolean = {
    val len = length

    var i = 0
    while (i < len && p(apply(i))) {
      i += 1
    }
    i == len
  }

  def forallWithIndex(p: (Double, Int) => Boolean): Boolean = {
    val len = length

    var i = 0
    while (i < len && p(apply(i), i)) {
      i += 1
    }
    i == len
  }

  def count(p: (Double) => Boolean): Int = {
    val len = length

    var count = 0
    var i = 0
    while (i < len) {
      if (p(apply(i))) count += 1
      i += 1
    }

    count
  }

  def ++ (that: Tuple): Tuple = {
    val thisLen = repr.length
    val thatLen = that.length

    val concat = new Array[Double](thisLen + thatLen)
    System.arraycopy(repr, 0, concat, 0, thisLen)
    System.arraycopy(that.repr, 0, concat, thisLen, thatLen)
    Tuple(concat)
  }

  def slice(from: Int, until: Int): Tuple = {
    val lo = scala.math.max(from, 0)
    val hi = scala.math.min(scala.math.max(until, 0), length)
    val len = scala.math.max(hi - lo, 0)

    val res = new Array[Double](len)
    System.arraycopy(repr, lo, res, 0, len)
    Tuple(res)
  }

  def toList: List[Double] = doubleArrayOps(repr).toList

  def toSet: Set[Double] = doubleArrayOps(repr).toSet

  override def toString: String = mkString("(", ", ", ")")

  def forceString: String = mkString("(", ", ", ")")

  def mkString(start: String, sep: String, end: String): String = {
    toList.mkString(start, sep, end)
  }


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
      res(i) = f(apply(i), rhs.repr.apply(i))
      i += 1
    }
    Tuple(res)
  }

  def unary_-(): Tuple = {
    map(-_)
  }

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
