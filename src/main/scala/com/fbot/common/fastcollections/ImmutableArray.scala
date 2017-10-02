package com.fbot.common.fastcollections

import com.fbot.common.fastcollections.index.ArrayIndex
import com.fbot.common.fastcollections.math.FastTupleDoubleMath

import scala.collection.mutable
import scala.reflect.ClassTag

/**
  *
  */
case class ImmutableArray[T](repr: mutable.WrappedArray[T]) extends AnyVal with FastArray[T, ImmutableArray] {

  def makeTransformed[B](x: mutable.WrappedArray[B]): ImmutableArray[B] = ImmutableArray(x)

}

object ImmutableArray {

  def apply[T: ClassTag](data0: T, dataRest: T*): ImmutableArray[T] = ImmutableArray[T]((data0 +: dataRest).toArray)

  def apply[T: ClassTag](data: TraversableOnce[T]): ImmutableArray[T] = ImmutableArray[T](data.toArray)

  def apply[T: ClassTag](data: Array[T]): ImmutableArray[T] = ImmutableArray[T](mutable.WrappedArray.make[T](data))

  def fill[T: ClassTag](n: Int)(elem: ⇒ T): ImmutableArray[T] = ImmutableArray(Array.fill[T](n)(elem))

  def empty[T: ClassTag]: ImmutableArray[T] = ImmutableArray(Array.empty[T])

  def range(start: Int, end: Int): ImmutableArray[Int] = ImmutableArray(Array.range(start, end))

  def indexRange(start: Int, end: Int): ImmutableArray[ArrayIndex] = range(start, end).map(i => ArrayIndex(i))

  implicit def builder[T: ClassTag](array: Array[T]): ImmutableArray[T] = ImmutableArray(array)

}


/**
  * Explicit specialization since:
  * - specialized does not work for value class ImmutableArray[T]
  * - since we want additional mixin trait for Double math (and don't wanna use implicit defs in the generic type)
  *
  * @param repr
  */
case class ImmutableDoubleArray(repr: mutable.WrappedArray[Double]) extends AnyVal with FastTupleDoubleMath[ImmutableDoubleArray] with FastArray[Double, ImmutableDoubleArray] {

  def makeTransformed[B](x: mutable.WrappedArray[B]): ImmutableArray[B] = ImmutableArray(x)

}

object ImmutableDoubleArray {

  def apply(data0: Double, dataRest: Double*): ImmutableDoubleArray = ImmutableDoubleArray((data0 +: dataRest).toArray)

  def apply(data: TraversableOnce[Double]): ImmutableDoubleArray = ImmutableDoubleArray(data.toArray)

  def apply(data: Array[Double]): ImmutableDoubleArray = ImmutableDoubleArray(mutable.WrappedArray.make[Double](data))

  def fill(n: Int)(elem: ⇒ Double): ImmutableDoubleArray = ImmutableDoubleArray(Array.fill[Double](n)(elem))

  def empty: ImmutableDoubleArray = ImmutableDoubleArray(Array.empty[Double])

  def range(start: Int, end: Int): ImmutableArray[Int] = ImmutableArray(Array.range(start, end))

  def indexRange(start: Int, end: Int): ImmutableArray[ArrayIndex] = range(start, end).map(i => ArrayIndex(i))

  implicit def builder(array: Array[Double]): ImmutableDoubleArray = ImmutableDoubleArray(array)

}


