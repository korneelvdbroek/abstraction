package com.fbot.common.fastcollections

import com.fbot.common.fastcollections.index.ArrayIndex

import scala.collection.{GenTraversableOnce, mutable}
import scala.reflect.ClassTag

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

  def apply(index: ArrayIndex): T = repr(index.i)

  def last: T = repr(length - 1)

  def foldLeft[B](z: B)(op: (B, T) => B): B = repr.foldLeft(z)(op)

  def forall(p: (T) => Boolean): Boolean = repr.forall(p)

  def forallWithIndex(p: (T, ArrayIndex) => Boolean): Boolean = {
    val len = length

    var i = 0
    while (i < len && p(repr(i), ArrayIndex(i))) i += 1
    i == len
  }

  def count(p: (T) => Boolean): Int = repr.count(p)

  def toArray(implicit evidence: scala.reflect.ClassTag[T]): Array[T] = repr.toArray

  def toList: List[T] = repr.toList

  def toSet: Set[T] = repr.toSet

  override def toString: String = repr.mkString("[", ",\n", "]")

}

