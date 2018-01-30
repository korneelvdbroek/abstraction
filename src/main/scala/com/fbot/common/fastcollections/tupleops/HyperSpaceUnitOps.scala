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
case class HyperSpaceUnitOps(repr: Array[Long]) {

  def length: Int = repr.length

  def dim: Int = length

  def isEmpty: Boolean = length == 0

  def nonEmpty: Boolean = !isEmpty


  def apply(index: Int): Long = repr(index)


  def head: Long = repr(0)

  def headOption: Option[Long] = if (nonEmpty) Some(head) else None

  def last: Long = repr(length - 1)

  def lastOption: Option[Long] = if (nonEmpty) Some(last) else None


  def foldLeft[B](z: B)(op: (B, Long) => B): B = foldl(0, length, z, op)

  @tailrec
  private def foldl[B](start: Int, end: Int, acc: B, op: (B, Long) => B): B = {
    if (start == end) {
      acc
    } else {
      foldl(start + 1, end, op(acc, apply(start)), op)
    }
  }

  def foldLeftOrBreak[B](z: B)(op: (B, Long) => (B, Boolean)): B = foldl(0, length, z, break = false, op)

  @tailrec
  private def foldl[B](start: Int, end: Int, acc: B, break: Boolean, op: (B, Long) => (B, Boolean)): B = {
    if (start == end || break) {
      acc
    } else {
      val (newAcc, newBreak) = op(acc, apply(start))
      foldl(start + 1, end, newAcc, newBreak, op)
    }
  }

  def forall(p: (Long) => Boolean): Boolean = {
    val len = length

    var i = 0
    while (i < len && p(apply(i))) {
      i += 1
    }
    i == len
  }

  def forallWithIndex(p: (Long, Int) => Boolean): Boolean = {
    val len = length

    var i = 0
    while (i < len && p(apply(i), i)) {
      i += 1
    }
    i == len
  }

  def count(p: (Long) => Boolean): Int = {
    val len = length

    var count = 0
    var i = 0
    while (i < len) {
      if (p(apply(i))) count += 1
      i += 1
    }

    count
  }

  def ++ (that: HyperSpaceUnit): HyperSpaceUnit = {
    val thisLen = repr.length
    val thatLen = that.length

    val concat = new Array[Long](thisLen + thatLen)
    System.arraycopy(repr, 0, concat, 0, thisLen)
    System.arraycopy(that.repr, 0, concat, thisLen, thatLen)
    HyperSpaceUnit(concat)
  }

  def toList: List[Long] = repr.toList

  def toSet: Set[Long] = repr.toSet

  override def toString: String = mkString("HyperSpaceUnit(", ", ", ")")

  def mkString(start: String, sep: String, end: String): String = {
    repr.mkString(start, sep, end)
  }

  def mkString(space: HyperSpace): String = {
    space.toCoordinate(HyperSpaceUnit(repr)).mkString("SpaceUnit(", ",", ")")
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

  def / (rhs: HyperSpaceUnit): HyperSpaceUnit = {
    elementWise(_ / _)(rhs)
  }

  private def elementWise(f: (Long, Long) => Long)(rhs: HyperSpaceUnit): HyperSpaceUnit = {
    val len = length
    val res: Array[Long] = new Array[Long](length)

    var i = 0
    while (i < len) {
      res(i) = f(apply(i), rhs.repr.apply(i))
      i += 1
    }
    HyperSpaceUnit(res)
  }

  def unary_-(): HyperSpaceUnit = {
    map(-_)
  }

  private def map(f: Long => Long): HyperSpaceUnit = {
    val len = length
    val res: Array[Long] = new Array[Long](length)

    var i = 0
    while (i < len) {
      res(i) = f(apply(i))
      i += 1
    }
    HyperSpaceUnit(res)
  }


}

