package com.fbot.common.fastcollections.tupleops

import com.fbot.common.fastcollections.HyperSpaceUnit
import com.fbot.common.fastcollections._
import com.fbot.common.hyperspace.HyperSpace

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
case class HyperSpaceUnitOps(repr: LiteWrappedArray[Long]) extends AnyVal {

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

  def mkString(start: String, sep: String, end: String): String = repr.mkString(start, sep, end)



  // new methods
  def dim: Int = length

  def mkString(space: HyperSpace): String = {
    space.toCoordinate(HyperSpaceUnit(repr.array)).mkString("SpaceUnit(", ",", ")")
  }

  def + (rhs: HyperSpaceUnit): HyperSpaceUnit = {
    elementWise(_ + _)(rhs)
  }

  def - (rhs: HyperSpaceUnit): HyperSpaceUnit = {
    elementWise(_ - _)(rhs)
  }

  def * (rhs: HyperSpaceUnit): HyperSpaceUnit = {
    elementWise(_ * _)(rhs)
  }

  @inline
  private def elementWise(f: (Long, Long) => Long)(rhs: HyperSpaceUnit): HyperSpaceUnit = {
    val len = length
    val res: Array[Long] = new Array[Long](len)

    var i = 0
    while (i < len) {
      res(i) = f(repr.array(i), rhs.repr.array(i))
      i += 1
    }
    HyperSpaceUnit(res)
  }

  def unary_-(): HyperSpaceUnit = {
    map(-_)
  }

  @inline
  private def map(f: Long => Long): HyperSpaceUnit = {
    val len = length
    val res: Array[Long] = new Array[Long](length)

    var i = 0
    while (i < len) {
      res(i) = f(repr.array(i))
      i += 1
    }
    HyperSpaceUnit(res)
  }


}

