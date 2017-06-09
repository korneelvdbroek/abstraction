package com.fbot.common.immutable

import scala.collection.mutable

/**
  * Copyright (C) 6/9/2017 - REstore NV
  *
  * Notes:
  * 0. this trait is written to extend a value class
  * 1. value class avoids time consuming boxing and also gives us the hashcode() and equals() of the data type
  * 2. since 1 already forces us to make repr public, might as well make it a case class (and get additional methods for free)
  */
trait ImmutableArrayOps[T, Self <: ImmutableArrayOps[T, Self]] extends Any {

  def repr: mutable.WrappedArray[T]

  def make(x: mutable.WrappedArray[T]): Self



  def make(x: Array[T]): Self = make(mutable.WrappedArray.make[T](x))

  def length: Int = repr.length

  def isEmpty: Boolean = repr.isEmpty

  def apply(index: ArrayIndex): T = repr(index.i)

  def last: T = repr(length - 1)

  def forall(p: (T) => Boolean): Boolean = repr.forall(p)

  def forallWithIndex(p: (T, ArrayIndex) => Boolean): Boolean = {
    val len = length

    var i = 0
    while (i < len && p(repr(i), ArrayIndex(i))) i += 1
    i == len
  }

  def toList: List[T] = repr.toList

  def toSet: Set[T] = repr.toSet

  override def toString: String = repr.mkString("[", ",\n", "]")

}

