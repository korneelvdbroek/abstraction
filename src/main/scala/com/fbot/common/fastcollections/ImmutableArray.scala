package com.fbot.common.fastcollections

import scala.collection.mutable
import scala.reflect.ClassTag

/**
  * Copyright (C) 6/10/2017 - REstore NV
  *
  */
case class ImmutableArray[T](repr: mutable.WrappedArray[T]) extends AnyVal with FastArray[T, ImmutableArray] {

  def makeTransformed[B](x: mutable.WrappedArray[B]): ImmutableArray[B] = ImmutableArray(x)

}

object ImmutableArray {

  def apply[T: ClassTag](data: T*): ImmutableArray[T] = ImmutableArray[T](data.toArray)

  def apply[T: ClassTag](data: Array[T]): ImmutableArray[T] = ImmutableArray[T](mutable.WrappedArray.make[T](data))

  def fill[T: ClassTag](n: Int)(elem: ⇒ T): ImmutableArray[T] = ImmutableArray(Array.fill[T](n)(elem))

  def empty[T: ClassTag]: ImmutableArray[T] = ImmutableArray(Array.empty[T])

}
