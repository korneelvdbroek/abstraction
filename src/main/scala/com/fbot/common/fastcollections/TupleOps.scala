package com.fbot.common.fastcollections

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
case class TupleOps(repr: Array[Double]) {

  def length: Int = repr.length

  def isEmpty: Boolean = length == 0

  def nonEmpty: Boolean = !isEmpty


  def apply(index: Int): Double = repr(index)


  def head: Double = repr(0)

  def headOption: Option[Double] = if (nonEmpty) Some(head) else None

  def last: Double = repr(length-1)

  def lastOption: Option[Double] = if (nonEmpty) Some(last) else None


  def foldLeft[B](z: B)(op: (B, Double) => B): B = foldl(0, length, z, op)   //repr.foldLeft(z)(op)

  def foldLeftOrBreak[B](z: B)(op: (B, Double) => (B, Boolean)): B = foldl(0, length, z, false, op)

  @tailrec
  private def foldl[B](start: Int, end: Int, z: B, break: Boolean, op: (B, Double) => (B, Boolean)): B = {
    if (start == end || break) {
      z
    } else {
      val () = this(start)
      foldl(start + 1, end, op(z, ), op)
    }
  }

  def forall(p: (Double) => Boolean): Boolean = repr.forall(p)

  def forallWithIndex(p: (Double, Int) => Boolean): Boolean = {
    val len = length

    var i = 0
    while (i < len && p(repr(i), i)) {
      i += 1
    }
    i == len
  }

  def count(p: (Double) => Boolean): Int = repr.count(p)

  def ++ (that: FastTuple[Double, Repr])(implicit evidence: scala.reflect.ClassTag[Double], builder: BuilderFromArray[Double, Repr]): Repr = {
    val thisLen = repr.toArray.length
    val thatLen = that.repr.length

    val x = new Array[Double](thisLen + thatLen)
    System.arraycopy(repr.toArray, 0, x, 0, thisLen)
    System.arraycopy(that.repr.toArray, 0, x, thisLen, thatLen)
    builder.result(x)
  }

  def toArray(implicit evidence: scala.reflect.ClassTag[Double]): Array[Double] = repr.toArray

  def toWrappedArray: mutable.WrappedArray[Double] = repr

  def toList: List[Double] = repr.toList

  def toSet: Set[Double] = repr.toSet

  override def toString: String = mkString("[", ", ", "]")

  def mkString(start: String, sep: String, end: String): String = {
    repr.mkString(start, sep, end)
  }


  def add(other: Tuple): Tuple = {
    val len = arr.length
    val res: Array[Double] = new Array[Double](len)

    var i: Int = 0
    while (i < len) {
      res(i) = arr(i) + other.arr(i)
      i += 1
    }

    Tuple(res)
  }


}
