package com.fbot.common.fastcollections.fastarrayops

import com.fbot.common.fastcollections.{ArrayIndex, ImmutableArray}
import shapeless.newtype.newtypeOps

import scala.collection.mutable
import scala.math.Ordering

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
case class FastIntArrayOps(repr: Array[Int]) extends FastArrayOps {

  type A = Int

  def sorted: ImmutableArray[Int] = {
    val len = length

    if (len <= 1) {
      ImmutableArray(repr)
    } else {
      // we make a copy, such that the unsorted ImmutableArray remains untouched
      val res: Array[Int] = new Array[Int](len)
      System.arraycopy(repr, 0, res, 0, len)
      java.util.Arrays.sort(res)
      ImmutableArray(res)
    }

  }

  def partialSort(k: Int): ImmutableArray[Int] = {
    val len = length
    val pq: mutable.PriorityQueue[Int] = new mutable.PriorityQueue[Int]()(Ordering.Int.lt)

    // load up the PQ
    var i = 0
    while (i < k && i < len) {
      pq.enqueue(repr(i))
      i += 1
    }

    // evaluate rest of array
    while (i < len) {
      if (repr(i) <= pq.head) {
        pq.dequeue()
        pq.enqueue(repr(i))
      }
      i += 1
    }

    ImmutableArray(pq.dequeueAll.reverse.toArray)
  }


  // Arithmetic
  def + (rhs: ImmutableArray[Int]): ImmutableArray[Int] = {
    elementWise(rhs, _ + _)
  }

  def - (rhs: ImmutableArray[Int]): ImmutableArray[Int] = {
    elementWise(rhs, _ - _)
  }

  def * (rhs: ImmutableArray[Int]): ImmutableArray[Int] = {
    elementWise(rhs, _ * _)
  }

  private def elementWise(rhs: ImmutableArray[Int], f: (Int, Int) => Int): ImmutableArray[Int] = {
    val len = length
    val res: Array[Int] = new Array[Int](length)

    var i = 0
    while (i < len) {
      res(i) = f(apply(ArrayIndex(i)), newtypeOps(rhs).repr.apply(i))
      i += 1
    }
    ImmutableArray[Int](res)
  }

  def unary_-(): ImmutableArray[Int] = {
    map(-_)
  }

  def sum: Int = {
    val len = length

    var i = 0
    var sum: Int = 0
    while (i < len) {
      sum += apply(ArrayIndex(i))
      i += 1
    }
    sum
  }

  def max: Int = {
    val len = length

    var i = 0
    var max = Int.MinValue
    while (i < len) {
      if (max < apply(ArrayIndex(i))) max = apply(ArrayIndex(i))
      i += 1
    }
    max
  }

}

