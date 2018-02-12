package com.fbot.common.fastcollections.core

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
// Not a value class, since we explicitly need to override equals() and hashcode(), so we do have boxing.
// The underlying Array[T], array, is only accessible to the fastcollections package
// since our type-safe newtype derivations might want to define some additional fast methods on top of it.
class LiteWrappedArray[@specialized(Int, Long, Double) A](private[fastcollections] val array: Array[A]) extends java.io.Serializable {

  /** The equals method for arbitrary sequences. Compares this sequence to
    * some other object.
    *
    * @param    that The object to compare the sequence to
    * @return `true` if `that` is a sequence that has the same elements as
    *         this sequence in the same order, `false` otherwise
    */
  override def equals(that: Any): Boolean = that match {
    case that: LiteWrappedArray[_] =>
      val len = array.length
      len == that.array.length && {
        var i = 0
        while (i < len && array(i) == that.array(i)) {
          i += 1
        }
        i == len
      }
    case _ => false
  }


  override def hashCode(): Int = scala.util.hashing.MurmurHash3.seqHash(array)


  def length: Int = array.length

  def isEmpty: Boolean = length == 0

  def nonEmpty: Boolean = !isEmpty


  // accessing elements
  def apply(index: Int): A = array(index.toInt)

  def head: A = array(0)

  def headOption: Option[A] = if (nonEmpty) Some(head) else None

  def last: A = array(length - 1)

  def lastOption: Option[A] = if (nonEmpty) Some(last) else None


  def foldLeft[B](z: B)(op: (B, A) => B): B = foldl(0, length, z, op)

  @tailrec
  private def foldl[B](start: Int, end: Int, acc: B, op: (B, A) => B): B = {
    if (start == end) {
      acc
    } else {
      foldl(start + 1, end, op(acc, array(start)), op)
    }
  }

  def foldLeftOrBreakWithIndex[B](z: B)(op: (B, A, Int) => (B, Boolean)): B = foldl(0, length, z, break = false, op)

  @tailrec
  private def foldl[B](start: Int, end: Int, acc: B, break: Boolean, op: (B, A, Int) => (B, Boolean)): B = {
    if (start == end || break) {
      acc
    } else {
      val (newAcc, newBreak) = op(acc, array(start), start)
      foldl(start + 1, end, newAcc, newBreak, op)
    }
  }

  def forall(p: (A) => Boolean): Boolean = {
    val len = length

    var i = 0
    while (i < len && p(array(i))) {
      i += 1
    }
    i == len
  }

  def forallWithIndex(p: (A, Int) => Boolean): Boolean = {
    val len = length

    var i = 0
    while (i < len && p(array(i), i)) {
      i += 1
    }
    i == len
  }

  def count(p: (A) => Boolean): Int = {
    val len = length

    var count = 0
    var i = 0
    while (i < len) {
      if (p(array(i))) count += 1
      i += 1
    }

    count
  }

  def ++ (that: LiteWrappedArray[A])(implicit evidence: ClassTag[A]): LiteWrappedArray[A] = {
    val thisLen = array.length
    val thatLen = that.length

    val concat = new Array[A](thisLen + thatLen)
    System.arraycopy(array, 0, concat, 0, thisLen)
    System.arraycopy(that.array, 0, concat, thisLen, thatLen)
    new LiteWrappedArray[A](concat)
  }

  def indexRange: LiteWrappedArray[Int] = {
    new LiteWrappedArray(Array.range(0, length))
  }

  def map[@specialized(Int, Long, Double) B: ClassTag](f: A => B): LiteWrappedArray[B] = {
    val len = length
    val res: Array[B] = new Array[B](len)

    var i = 0
    while (i < len) {
      res(i) = f(array(i))
      i += 1
    }
    new LiteWrappedArray[B](res)
  }

  def mapWithIndex[@specialized(Int, Long, Double) B: ClassTag](f: (A, Int) ⇒ B): LiteWrappedArray[B] = {
    val len = length
    val res: Array[B] = new Array[B](len)

    var i = 0
    while (i < len) {
      res(i) = f(array(i), i)
      i += 1
    }
    new LiteWrappedArray[B](res)
  }

  private def flattenArray[@specialized(Double, Int, Long) B: ClassTag](unFlattened: Array[LiteWrappedArray[B]], flattenedLen: Int): LiteWrappedArray[B] = {
    val len = length
    val res: Array[B] = new Array[B](flattenedLen)

    var i = 0
    var pos = 0
    while (i < len) {
      val element = unFlattened(i)
      System.arraycopy(element.toArray, 0, res, pos, element.length)
      pos += element.length
      i += 1
    }
    new LiteWrappedArray[B](res)
  }

  def flatMap(f: A => LiteWrappedArray[A])(implicit evidence: ClassTag[A]): LiteWrappedArray[A] = {
    val len = length
    val unFlattened: Array[LiteWrappedArray[A]] = new Array[LiteWrappedArray[A]](len)

    var i = 0
    var flattenedLength = 0
    while (i < len) {
      val mapped = f(array(i))
      unFlattened(i) = mapped
      flattenedLength += mapped.length
      i += 1
    }

    flattenArray(unFlattened, flattenedLength)
  }

  def flatMapWithIndex[B: ClassTag](f: (A, Int) => LiteWrappedArray[B]): LiteWrappedArray[B] = {
    val len = length
    val unFlattened: Array[LiteWrappedArray[B]] = new Array[LiteWrappedArray[B]](len)

    var i = 0
    var flattenedLength = 0
    while (i < len) {
      val mapped = f(array(i), i)
      unFlattened(i) = mapped
      flattenedLength += mapped.length
      i += 1
    }

    flattenArray(unFlattened, flattenedLength)
  }

  def take(k: Int)(implicit evidence: ClassTag[A]): LiteWrappedArray[A] = {
    val res = new Array[A](k)
    System.arraycopy(array, 0, res, 0, k)
    new LiteWrappedArray(res)
  }

  def slice(from: Int, until: Int)(implicit evidence: ClassTag[A]): LiteWrappedArray[A] = {
    val lo = scala.math.max(from, 0)
    val hi = scala.math.min(scala.math.max(until, 0), length)
    val len = scala.math.max(hi - lo, 0)

    val res = new Array[A](len)
    System.arraycopy(array, lo, res, 0, len)
    new LiteWrappedArray(res)
  }

  def filter(p: A ⇒ Boolean)(implicit evidence: ClassTag[A]): LiteWrappedArray[A] = {
    val len = length
    val temp: Array[A] = new Array[A](len)

    var i = 0
    var pos = 0
    while (i < len) {
      val x = array(i)
      if (p(x)) {
        temp(pos) = x
        pos += 1
      }
      i += 1
    }

    // resize array
    val res: Array[A] = new Array[A](pos)
    System.arraycopy(temp, 0, res, 0, pos)
    new LiteWrappedArray[A](res)
  }


  def filterByIndex(p: Int => Boolean)(implicit evidence: ClassTag[A]): LiteWrappedArray[A] = {
    val len = length
    val temp: Array[A] = new Array[A](len)

    var i = 0
    var pos = 0
    while (i < len) {
      if (p(i)) {
        temp(pos) = array(i)
        pos += 1
      }
      i += 1
    }

    // resize array
    val res: Array[A] = new Array[A](pos)
    System.arraycopy(temp, 0, res, 0, pos)
    new LiteWrappedArray[A](res)
  }


  def filterNot(p: A ⇒ Boolean)(implicit evidence: ClassTag[A]): LiteWrappedArray[A] = {
    filter(!p(_))
  }

  def groupBy[Key: ClassTag](f: A ⇒ Key): Map[Key, LiteWrappedArray[A]] = {
    array.groupBy(f).mapValues(grouped => new LiteWrappedArray(grouped))
  }


  // conversions
  def toArray: Array[A] = array

  def toList: List[A] = array.toList

  def toSet: Set[A] = array.toSet


  // Strings
  override def toString: String = mkString("LiteArray(", ", ", ")")

  def mkString(start: String, sep: String, end: String): String = {
    array.mkString(start, sep, end)
  }

}
