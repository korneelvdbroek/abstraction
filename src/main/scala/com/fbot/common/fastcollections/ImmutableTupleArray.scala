package com.fbot.common.fastcollections

import com.fbot.common.hyperspace.TupleX$

import scala.annotation.tailrec
import scala.collection.mutable

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
case class ImmutableTupleArray(points: mutable.WrappedArray[ImmutableArray[Double]]) {

  def length: Int = {
    if (points.isEmpty) 0 else points.head.repr.length
  }

  def isEmpty: Boolean = points.isEmpty || points.head.repr.isEmpty

  def apply(index: Int): TupleX = TupleX(points.map(_(index)))

  private def applyUnBoxed(index: Int): mutable.WrappedArray[Double] = points.map(_(index))

  def head: TupleX = TupleX(points.map(_.head))

  def headOption: Option[TupleX] = if (isEmpty) None else Some(head)

  def last: TupleX = TupleX(points.map(_.last))

  def foreach(f: mutable.WrappedArray[Double] ⇒ Unit): Unit = {
    val len = length

    var i: Int = 0
    while (i < len) {
      f(applyUnBoxed(i))
      i += 1
    }
  }

  def foldLeft[Acc](z: Acc)(op: (Acc, TupleX) => Acc): Acc = {
    var acc = z
    foreach(point => {
      acc = op(acc, TupleX(point))
    })
    acc
  }

  def foldLeftOrBreak[Acc](z: Acc)(op: (Acc, TupleX) => (Acc, Boolean)): Acc = foldl(0, length, (z, false), op)

  @tailrec
  private def foldl[Acc](start: Int, end: Int, z: (Acc, Boolean), op: (Acc, TupleX) => (Acc, Boolean)): Acc = {
    if (start == end || z._2) {
      z._1
    } else {
      foldl(start + 1, end, op(z._1, this(start)), op)
    }
  }

  def forall(p: (TupleX) => Boolean): Boolean = {
    val len = length

    var i = 0
    while (i < len && p(this(i))) {
      i += 1
    }
    i == len
  }

  def forallWithIndex(p: (TupleX, Int) => Boolean): Boolean = {
    val len = length

    var i = 0
    while (i < len && p(this(i), i)) {
      i += 1
    }
    i == len
  }

  def count(p: (TupleX) => Boolean): Int = {
    var counter = 0
    foreach(point => {
      if (p(TupleX(point))) counter += 1
    })
    counter
  }


  // TODO: continue here
//  def ++ (that: ImmutableTupleArray)(implicit evidence: scala.reflect.ClassTag[Double], builder: BuilderFromArray[Double, Repr]): ImmutableTupleArray = {
//    val thisLen = points.toArray.length
//    val thatLen = that.repr.length
//
//    val x = new Array[Double](thisLen + thatLen)
//    System.arraycopy(points.toArray, 0, x, 0, thisLen)
//    System.arraycopy(that.repr.toArray, 0, x, thisLen, thatLen)
//    builder.result(x)
//  }
//
//  def toArray(implicit evidence: scala.reflect.ClassTag[Double]): Array[Tuple] = points.toArray
//
//  def toWrappedArray: mutable.WrappedArray[Tuple] = points
//
//  def toList: List[Tuple] = points.toList
//
//  def toSet: Set[Tuple] = points.toSet
//
//  override def toString: String = mkString("[", ", ", "]")
//
//  def mkString(start: String, sep: String, end: String): String = {
//    points.mkString(start, sep, end)
//  }

  // TODO: add in FastArray stuff (steal from FastArray2Zipped)
  // TODO: copy FastArray2Zipped to concat 2 ImmutableTupleArrays
  // TODO: move some of the implementation of FastTuple and FastArray to ImmutableArray and Tuple, so they can be used as interface for this class

}
