package com.fbot.common.fastcollections

import scala.annotation.tailrec
import scala.collection.mutable

/**
  * @tparam A    the collection element type.
  * @tparam Repr the actual type of the element container.
  *
  */
trait FastTuple[A, Repr] {

  def repr: mutable.WrappedArray[A]


  def length: Int = repr.length

  def isEmpty: Boolean = repr.isEmpty

  def apply(index: Int): A = repr(index)

  def head: A = repr.head

  def headOption: Option[A] = repr.headOption

  def last: A = repr.last

  def foldLeft[B](z: B)(op: (B, A) => B): B = repr.foldLeft(z)(op)

  def foldLeftOrBreak[B](z: B)(op: (B, A) => (B, Boolean)): B = foldl(0, length, (z, false), op)

  @tailrec
  private def foldl[B](start: Int, end: Int, z: (B, Boolean), op: (B, A) => (B, Boolean)): B = {
    if (start == end || z._2) {
      z._1
    } else {
      foldl(start + 1, end, op(z._1, this (start)), op)
    }
  }

  def forall(p: (A) => Boolean): Boolean = repr.forall(p)

  def forallWithIndex(p: (A, Int) => Boolean): Boolean = {
    val len = length

    var i = 0
    while (i < len && p(repr(i), i)) {
      i += 1
    }
    i == len
  }

  def count(p: (A) => Boolean): Int = repr.count(p)

  def ++ (that: FastTuple[A, Repr])(implicit evidence: scala.reflect.ClassTag[A], builder: BuilderFromArray[A, Repr]): Repr = {
    val thisLen = repr.toArray.length
    val thatLen = that.repr.length

    val x = new Array[A](thisLen + thatLen)
    System.arraycopy(repr.toArray, 0, x, 0, thisLen)
    System.arraycopy(that.repr.toArray, 0, x, thisLen, thatLen)
    builder.result(x)
  }

  def toArray(implicit evidence: scala.reflect.ClassTag[A]): Array[A] = repr.toArray

  def toWrappedArray: mutable.WrappedArray[A] = repr

  def toList: List[A] = repr.toList

  def toSet: Set[A] = repr.toSet

  override def toString: String = mkString("[", ", ", "]")

  def mkString(start: String, sep: String, end: String): String = {
    repr.mkString(start, sep, end)
  }

}


trait BuilderFromArray[Elem, To] {

  def result(array: Array[Elem]): To

  //def result[Elem: ClassTag](array: mutable.WrappedArray[Elem]): To = result(array.toArray)

}

