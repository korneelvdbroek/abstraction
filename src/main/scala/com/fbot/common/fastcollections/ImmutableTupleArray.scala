package com.fbot.common.fastcollections

import com.fbot.common.fastcollections.core.LiteWrappedArray

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.immutable
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

  def dim: Int = coordinates.length

  def length: Int = {
    if (coordinates.length == 0) {
      0
    } else {
      coordinates(0).length
    }
  }

  def isEmpty: Boolean = coordinates.length == 0 || coordinates(0).isEmpty

  def apply(index: ArrayIndex): Tuple = {
    val dimension = dim
    val res: Array[Double] = new Array[Double](dimension)

    var d = 0
    while (d < dimension) {
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


  def ++ (that: ImmutableTupleArray): ImmutableTupleArray = {
    assert(this.dim == that.dim, "Cannot concatenate ImmutableArray of Tuples since Tuple dimensions do not match")

    val dimension = dim
    val res: Array[ImmutableArray[Double]] = new Array[ImmutableArray[Double]](dimension)

    var d = 0
    while (d < dimension) {
      res.update(d, coordinates(d) ++ that.coordinates(d))
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
    val thisDim = dim
    val thatDim = that.dim
    val res: Array[ImmutableArray[Double]] = new Array[ImmutableArray[Double]](thisDim + thatDim)

    System.arraycopy(coordinates, 0, res, 0, thisDim)
    System.arraycopy(that.coordinates, 0, res, thisDim, thatDim)
    ImmutableTupleArray(res)
  }

  def map(f: Tuple ⇒ Tuple): ImmutableTupleArray = {
    val len = length
    val dimension = dim

    var d: Int = 0
    val res: Array[Array[Double]] = new Array[Array[Double]](dimension)
    while (d < dimension) {
      res(d) = new Array[Double](len)
      d += 1
    }

    var i: Int = 0
    while (i < len) {
      var d: Int = 0
      while (d < dimension) {
        res(d)(i) = f(apply(ArrayIndex(i))).apply(d)
        d += 1
      }
      i += 1
    }

    ImmutableTupleArray(res.map(arr => new LiteWrappedArray(arr)).asInstanceOf[Array[ImmutableArray[Double]]])
  }

  def mapWithIndex(f: (Tuple, Int) ⇒ Tuple): ImmutableTupleArray = {
    val len = length
    val dimension = dim

    var d: Int = 0
    val res: Array[Array[Double]] = new Array[Array[Double]](dimension)
    while (d < dimension) {
      res(d) = new Array[Double](len)
      d += 1
    }

    var i: Int = 0
    while (i < len) {
      var d: Int = 0
      while (d < dimension) {
        res(d)(i) = f(apply(ArrayIndex(i)), i).apply(d)
        d += 1
      }
      i += 1
    }

    ImmutableTupleArray(res.map(arr => new LiteWrappedArray(arr)).asInstanceOf[Array[ImmutableArray[Double]]])
  }

  def take(k: Int): ImmutableTupleArray = {
    val dimension = dim

    val res: Array[ImmutableArray[Double]] = new Array[ImmutableArray[Double]](dimension)

    var d: Int = 0
    while (d < dimension) {
      res(d) = coordinates(d).take(k)
      d += 1
    }

    ImmutableTupleArray(res)
  }

  def slice(from: Int, until: Int): ImmutableTupleArray = {
    val dimension = dim

    val res: Array[ImmutableArray[Double]] = new Array[ImmutableArray[Double]](dimension)

    var d: Int = 0
    while (d < dimension) {
      res(d) = coordinates(d).slice(from, until)
      d += 1
    }

    ImmutableTupleArray(res)
  }

  def indexRange: ImmutableArray[ArrayIndex] = {
    ImmutableArray(Array.range(0, length).map(ArrayIndex(_)))
  }

  def filter(p: Tuple ⇒ Boolean): ImmutableTupleArray = {
    select(filterIndices(p))
  }

  def filterNot(p: Tuple ⇒ Boolean): ImmutableTupleArray = {
    select(filterIndices(!p(_)))
  }

  def select(indices: ImmutableArray[ArrayIndex]): ImmutableTupleArray = {
    val len = indices.length
    val dimension = dim

    var d: Int = 0
    val res: Array[Array[Double]] = new Array[Array[Double]](dimension)
    while (d < dimension) {
      res(d) = new Array[Double](len)
      d += 1
    }

    var iFiltered: Int = 0
    while (iFiltered < len) {
      var d: Int = 0
      while (d < dimension) {
        res(d)(iFiltered) = apply(indices(ArrayIndex(iFiltered)))(d)
        d += 1
      }
      iFiltered += 1
    }

    ImmutableTupleArray(res.map(arr => new LiteWrappedArray(arr)).asInstanceOf[Array[ImmutableArray[Double]]])
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
    val dimension = dim
    val m = mutable.Map.empty[Key, Array[mutable.Builder[Double, Array[Double]]]]

    // make mutable Map
    var i: Int = 0
    while (i < len) {
      val elem = apply(ArrayIndex(i))
      val key = f(elem)

      // find key, or create new entry (Key, (ArrayBuilder, ArrayBuilder, ...))
      val bldr = m.getOrElseUpdate(key, {
        val emptyTupleArray = new Array[mutable.Builder[Double, Array[Double]]](dimension)
        var d = 0
        while (d < dimension) {
          emptyTupleArray(d) = new mutable.ArrayBuilder.ofDouble
          d += 1
        }
        emptyTupleArray
      })

      // add actual tuple
      var d = 0
      while (d < dimension) {
        bldr(d) += elem(d)
        d += 1
      }

      i += 1
    }

    // copy to immutable Map
    val b = immutable.Map.newBuilder[Key, ImmutableTupleArray]
    for ((k, v) <- m) {
      var d = 0
      val res = new Array[ImmutableArray[Double]](dimension)
      while (d < dimension) {
        res(d) = ImmutableArray(v(d).result)
        d += 1
      }
      b += ((k, ImmutableTupleArray(res)))
    }

    b.result
  }


  // Strings
  override def toString: String = mkString("[", ", ", "]")

  def mkString(start: String, sep: String, end: String): String = {
    toArray.mkString(start, sep, end)
  }
}


object ImmutableTupleArray {

  def fill(n: Int)(elem: ⇒ Tuple): ImmutableTupleArray = fromTuples(ImmutableArray(Array.fill[Tuple](n)(elem)))

  def fromTuples(tuples: ImmutableArray[Tuple]): ImmutableTupleArray = {
    val len = tuples.length

    if (len == 0) {
      ImmutableTupleArray.empty(0)
    } else {
      val dim = tuples.head.dim

      var d: Int = 0
      val res: Array[Array[Double]] = new Array[Array[Double]](dim)
      while (d < dim) {
        res(d) = new Array[Double](len)
        d += 1
      }

      var i = 0
      while (i < len) {
        var d = 0
        while (d < dim) {
          res(d)(i) = tuples(ArrayIndex(i))(d)
          d += 1
        }
        i += 1
      }

      ImmutableTupleArray(res.map(arr => new LiteWrappedArray(arr)).asInstanceOf[Array[ImmutableArray[Double]]])
    }
  }

  def empty(dim: Int, len: Int = 0): ImmutableTupleArray = {
    var d: Int = 0
    val res: Array[Array[Double]] = new Array[Array[Double]](dim)
    while (d < dim) {
      res(d) = new Array[Double](len)
      d += 1
    }
    ImmutableTupleArray(res.map(arr => new LiteWrappedArray(arr)).asInstanceOf[Array[ImmutableArray[Double]]])
  }

}