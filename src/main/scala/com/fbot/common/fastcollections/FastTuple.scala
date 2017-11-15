package com.fbot.common.fastcollections

import com.fbot.common.fastcollections.index.ArrayIndex

import scala.annotation.tailrec
import scala.collection.mutable

/**
  * Notes:
  * 0. this trait is written to extend a value class (hence it extends from Any)
  * 1. value class avoids time consuming boxing of the Array and also gives us a good equals() and hashcode() from WrappedArray
  * 2. since 1 already forces us to make repr public, might as well make it a case class (and get additional methods for free)
  */
trait FastTuple[T, Self <: FastTuple[T, Self]] extends Any {

  def repr: mutable.WrappedArray[T]

  def make(x: mutable.WrappedArray[T]): Self


  def makeFromArray(x: Array[T]): Self = make(mutable.WrappedArray.make[T](x))

  def length: Int = repr.length

  def isEmpty: Boolean = repr.isEmpty

  def apply(index: ArrayIndex): T = repr(index.toInt)

  def head: T = repr.head

  def last: T = repr.last

  def foldLeft[B](z: B)(op: (B, T) => B): B = repr.foldLeft(z)(op)

  def foldLeftOrBreak[B](z: B)(op: (B, T) => (B, Boolean)): B = foldl(ArrayIndex(0), ArrayIndex(length), (z, false), op)

  @tailrec
  private def foldl[B](start: ArrayIndex, end: ArrayIndex, z: (B, Boolean), op: (B, T) => (B, Boolean)): B = {
    if (start == end || z._2) {
      z._1
    } else {
      foldl(start.next, end, op(z._1, this (start)), op)
    }
  }

  def forall(p: (T) => Boolean): Boolean = repr.forall(p)

  def forallWithIndex(p: (T, ArrayIndex) => Boolean): Boolean = {
    val len = length

    var i = 0
    while (i < len && p(repr(i), ArrayIndex(i))) {
      i += 1
    }
    i == len
  }

  def count(p: (T) => Boolean): Int = repr.count(p)

  def ++ (that: Self)(implicit evidence: scala.reflect.ClassTag[T]): Self = {
    val thisLen = repr.toArray.length
    val thatLen = that.repr.toArray.length

    val x = new Array[T](thisLen + thatLen)
    System.arraycopy(repr.toArray, 0, x, 0, thisLen)
    System.arraycopy(that.repr.toArray, 0, x, thisLen, thatLen)
    make(x)
  }

  def toArray(implicit evidence: scala.reflect.ClassTag[T]): Array[T] = repr.toArray

  def toList: List[T] = repr.toList

  def toSet: Set[T] = repr.toSet

  override def toString: String = mkString("[", ", ", "]")

  def mkString(start: String, sep: String, end: String): String = {
    repr.mkString(start, sep, end)
  }

}

