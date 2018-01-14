package com.fbot.common.fastcollections

import scala.annotation.tailrec
import shapeless.newtype.newtypeOps

import scala.collection.{immutable, mutable}
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
case class ImmutableTupleArray(coordinates: Array[ImmutableArray[Double]]) {

  def dimension: Int = coordinates.length

  def length: Int = {
    if (coordinates.length == 0) 0 else coordinates(0).length
  }

  def isEmpty: Boolean = coordinates.length == 0 || coordinates(0).isEmpty

  def apply(index: ArrayIndex): Tuple = {
    val dim = dimension
    val res: Array[Double] = new Array[Double](dim)

    var d = 0
    while (d < dim) {
      res(d) = coordinates(d)(index)
      d += 1
    }
    Tuple(res)
  }

  def coordinate(d: Int): ImmutableArray[Double] = {
    coordinates(d)
  }

  def head: Tuple = apply(ArrayIndex(0))

  def headOption: Option[Tuple] = if (isEmpty) None else Some(head)

  def last: Tuple = apply(ArrayIndex(length - 1))

  def foreach(f: Tuple ⇒ Unit): Unit = {
    val len = length

    var i: Int = 0
    while (i < len) {
      f(apply(ArrayIndex(i)))
      i += 1
    }
  }

  def foldLeft[Acc](z: Acc)(op: (Acc, Tuple) => Acc): Acc = {
    var acc = z
    foreach(tuple => {
      acc = op(acc, tuple)
    })
    acc
  }

  def foldLeftOrBreak[Acc](z: Acc)(op: (Acc, Tuple) => (Acc, Boolean)): Acc = foldl(0, length, z, false, op)

  @tailrec
  private def foldl[Acc](start: Int, end: Int, z: Acc, break: Boolean, op: (Acc, Tuple) => (Acc, Boolean)): Acc = {
    if (start == end || break) {
      z
    } else {
      val (newAcc, newBreak) = op(z, apply(ArrayIndex(start)))
      foldl(start + 1, end, newAcc, newBreak, op)
    }
  }

  def forall(p: Tuple => Boolean): Boolean = {
    val len = length

    var i = 0
    while (i < len && p(apply(ArrayIndex(i)))) {
      i += 1
    }
    i == len
  }

  def forallWithIndex(p: (Tuple, Int) => Boolean): Boolean = {
    val len = length

    var i = 0
    while (i < len && p(apply(ArrayIndex(i)), i)) {
      i += 1
    }
    i == len
  }

  def count(p: Tuple => Boolean): Int = {
    var counter = 0
    foreach(tuple => {
      if (p(tuple)) counter += 1
    })
    counter
  }


  def ++(that: ImmutableTupleArray): ImmutableTupleArray = {
    assert(this.dimension == that.dimension, "Cannot concatenate ImmutableArray of Tuples since Tuple dimensions do not match")

    val dim = dimension
    val res: Array[ImmutableArray[Double]] = new Array[ImmutableArray[Double]](dim)

    var d = 0
    while (d < dim) {
      res(d) = coordinates(d) ++ that.coordinates(d)
      d += 1
    }

    ImmutableTupleArray(res)
  }


  // conversions
  def toArray: Array[Tuple] = {
    val len = length
    val res: Array[Tuple] = new Array[Tuple](len)

    var i = 0
    while (i < len) {
      res(i) = apply(ArrayIndex(i))
      i += 1
    }
    res
  }



  // non-tuple operations
  def extend(that: ImmutableTupleArray): ImmutableTupleArray = {
    val thisDim = dimension
    val thatDim = that.dimension
    val res: Array[ImmutableArray[Double]] = new Array[ImmutableArray[Double]](thisDim + thatDim)

    System.arraycopy(coordinates, 0, res, 0, thisDim)
    System.arraycopy(that.coordinates, 0, res, thisDim, thatDim)
    ImmutableTupleArray(res)
  }

  def map(f: Tuple ⇒ Tuple): ImmutableTupleArray = {
    val len = length
    val dim = dimension

    var d: Int = 0
    val res: Array[Array[Double]] = new Array[Array[Double]](dim)
    while (d < dim) {
      res(d) = new Array[Double](len)
      d += 1
    }

    var i: Int = 0
    while (i < len) {
      var d: Int = 0
      while (d < dim) {
        res(d)(i) = f(apply(ArrayIndex(i))).apply(d)
        d += 1
      }
      i += 1
    }

    ImmutableTupleArray(res.asInstanceOf[Array[ImmutableArray[Double]]])
  }

  def mapWithIndex(f: (Tuple, Int) ⇒ Tuple): ImmutableTupleArray = {
    val len = length
    val dim = dimension

    var d: Int = 0
    val res: Array[Array[Double]] = new Array[Array[Double]](dim)
    while (d < dim) {
      res(d) = new Array[Double](len)
      d += 1
    }

    var i: Int = 0
    while (i < len) {
      var d: Int = 0
      while (d < dim) {
        res(d)(i) = f(apply(ArrayIndex(i)), i).apply(d)
        d += 1
      }
      i += 1
    }

    ImmutableTupleArray(res.asInstanceOf[Array[ImmutableArray[Double]]])
  }

  def take(k: Int): ImmutableTupleArray = {
    val dim = dimension

    val res: Array[ImmutableArray[Double]] = new Array[ImmutableArray[Double]](dim)

    var d: Int = 0
    while (d < dim) {
      res(d) = coordinates(d).take(k)
      d += 1
    }

    ImmutableTupleArray(res)
  }

  def slice(from: Int, until: Int): ImmutableTupleArray = {
    val dim = dimension

    val res: Array[ImmutableArray[Double]] = new Array[ImmutableArray[Double]](dim)

    var d: Int = 0
    while (d < dim) {
      res(d) = coordinates(d).slice(from, until)
      d += 1
    }

    ImmutableTupleArray(res)
  }

  def filter(p: Tuple ⇒ Boolean): ImmutableTupleArray = {
    select(filterIndices(p))
  }

  def filterNot(p: Tuple ⇒ Boolean): ImmutableTupleArray = {
    select(filterIndices(!p(_)))
  }

  def select(indices: ImmutableArray[ArrayIndex]): ImmutableTupleArray = {
    val len = indices.length
    val dim = dimension

    var d: Int = 0
    val res: Array[Array[Double]] = new Array[Array[Double]](dim)
    while (d < dim) {
      res(d) = new Array[Double](len)
      d += 1
    }

    var iFiltered: Int = 0
    while (iFiltered < len) {
      var d: Int = 0
      while (d < dim) {
        res(d)(iFiltered) = apply(indices(ArrayIndex(iFiltered)))(d)
        d += 1
      }
      iFiltered += 1
    }

    ImmutableTupleArray(res.asInstanceOf[Array[ImmutableArray[Double]]])
  }

  def filterIndices(p: Tuple ⇒ Boolean): ImmutableArray[ArrayIndex] = {
    val len = length
    val temp: Array[ArrayIndex] = new Array[ArrayIndex](len)

    var i = 0
    var pos = 0
    while (i < len) {
      val x = apply(ArrayIndex(i))
      if (p(x)) {
        temp(pos) = ArrayIndex(i)
        pos += 1
      }
      i += 1
    }

    // resize array
    val res: Array[ArrayIndex] = new Array[ArrayIndex](pos)
    System.arraycopy(temp, 0, res, 0, pos)
    ImmutableArray[ArrayIndex](res)
  }


  def groupBy[Key: ClassTag](f: Tuple ⇒ Key): Map[Key, ImmutableTupleArray] = {
    val len = length
    val dim = dimension
    val m = mutable.Map.empty[Key, Array[mutable.Builder[Double, Array[Double]]]]

    // make mutable Map
    var i: Int = 0
    while (i < len) {
      val elem = apply(ArrayIndex(i))
      val key = f(elem)

      // find key, or create new entry (Key, (ArrayBuilder, ArrayBuilder, ...))
      val bldr = m.getOrElseUpdate(key, {
        val emptyTupleArray = new Array[mutable.Builder[Double, Array[Double]]](dim)
        var d = 0
        while (d < dim) {
          emptyTupleArray(d) = new mutable.ArrayBuilder.ofDouble
          d += 1
        }
        emptyTupleArray
      })

      // add actual tuple
      var d = 0
      while (d < dim) {
        bldr(d) += elem(d)
        d += 1
      }

      i += 1
    }

    // copy to immutable Map
    val b = immutable.Map.newBuilder[Key, ImmutableTupleArray]
    for ((k, v) <- m) {
      var d = 0
      val res = new Array[ImmutableArray[Double]](dim)
      while (d < dim) {
        res(d) = ImmutableArray(v(d).result)
        d += 1
      }
      b += ((k, ImmutableTupleArray(res)))
    }

    b.result
  }


  //  def sortWith(lt: (A, A) ⇒ Boolean)(implicit evidence: scala.reflect.ClassTag[A], builder: BuilderFromArray[A, Repr]): Repr = builder
  //    .result(repr.sortWith(lt).toArray)
  //
  //  def sortBy[B](f: (A) ⇒ B)(implicit evidence: scala.reflect.ClassTag[A], ord: Ordering[B], builder: BuilderFromArray[A, Repr]): Repr = builder
  //    .result(repr.sortBy(f).toArray)

  def partialSort(k: Int): ImmutableTupleArray = {
    val len = length

    val array = repr.toArray
    val pq: mutable.PriorityQueue[Tuple] = new mutable.PriorityQueue[Tuple]()(ordering)

    // load up the PQ
    var i: Int = 0
    while (i < k && i < len) {
      pq.enqueue(array(i))
      i += 1
    }

    // evaluate rest of array
    while (i < len) {
      //println(s"${ array(i) } ${ ordering.compare(array(i), pq.head) }")
      if (ordering.compare(array(i), pq.head) <= 0) {
        pq.dequeue()
        pq.enqueue(array(i))
      }
      i += 1
    }

    builder.result(pq.dequeueAll.reverse.toArray)
  }


  // Strings
  override def toString: String = mkString("[", ", ", "]")

  def mkString(start: String, sep: String, end: String): String = {
    toArray.mkString(start, sep, end)
  }
}


object ImmutableTupleArray {

}