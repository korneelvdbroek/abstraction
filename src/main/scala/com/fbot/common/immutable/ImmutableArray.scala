package com.fbot.common.immutable

import scala.reflect.ClassTag
import BooleanArrayMath._
import ZippedImmutableArray2._
import ArrayIndex._

import scala.collection.mutable

/**
  * Copyright (C) 6/3/2017 - REstore NV
  *
  */
case class ImmutableArray[T: ClassTag](repr: mutable.WrappedArray[T]) {

  def ++[T: ClassTag](that: ImmutableArray[T]): ImmutableArray[T] = {
    val thisLen = this.repr.toArray.length
    val thatLen = that.repr.toArray.length
    val x = new Array[T](thisLen + thatLen)
    System.arraycopy(this.repr.toArray, 0, x, 0, thisLen)
    System.arraycopy(that.repr.toArray, 0, x, thisLen, thatLen)
    new ImmutableArray(x)
  }

  def length: Int = repr.length

  def isEmpty: Boolean = repr.isEmpty

  def apply(index: ArrayIndex): T = repr(index.i)

  def last: T = repr(length - 1)

  def forall(p: (T) => Boolean): Boolean = repr.forall(p)

  def map[B: ClassTag](f: (T) ⇒ B): ImmutableArray[B] = new ImmutableArray(repr.map(f))

  def mapWithIndex[B: ClassTag](f: (T, ArrayIndex) ⇒ B): ImmutableArray[B] = {
    val len = length
    val mapped = new Array[B](len)

    var i = 0
    while (i < len) {
      mapped(i) = f(this(ArrayIndex(i)), ArrayIndex(i))
      i += 1
    }

    new ImmutableArray(mapped)
  }

  def sortWith(lt: (T, T) ⇒ Boolean): ImmutableArray[T] = new ImmutableArray(repr.sortWith(lt))

  def sortBy[B](f: (T) ⇒ B)(implicit ord: math.Ordering[B]): ImmutableArray[T] = new ImmutableArray(repr.sortBy(f))

  def take(n: Int): ImmutableArray[T] = {
    val x = new Array[T](n)
    System.arraycopy(this.repr.toArray[T], 0, x, 0, n)
    new ImmutableArray(x)
  }

  def flatten[U: ClassTag](implicit asArray: (T) ⇒ ImmutableArray[U]): ImmutableArray[U] = {
    val n = repr.map(elem => asArray(elem).length).sum
    val x = new Array[U](n)

    var i = 0
    repr foreach { elem =>
      val array = asArray(elem)
      System.arraycopy(array.repr.toArray[U], 0, x, i, array.length)
      i += array.length
    }

    new ImmutableArray(x)
  }

  def filter(p: (T) ⇒ Boolean): ImmutableArray[T] = new ImmutableArray(repr.filter(p))

  def filterNot(p: (T) ⇒ Boolean): ImmutableArray[T] = new ImmutableArray(repr.filterNot(p))

  def unzippedGroupBy[K: ClassTag](f: (T) ⇒ K): UnzippedMap[K, ImmutableArray[T]] = {
    val unzipped = repr.groupBy(f).mapValues(new ImmutableArray(_)).unzip
    UnzippedMap(unzipped._1, unzipped._2)
  }

  def toList: List[T] = repr.toList

  override def toString: String = repr.mkString("[", ",\n", "]")

  def indexRange: ImmutableArray[ArrayIndex] = ImmutableArray(Array.range(0, repr.length).map(ArrayIndex(_)))

}


object ImmutableArray {

  def apply[T: ClassTag](data: T*): ImmutableArray[T] = ImmutableArray[T](data.toArray)

  def apply[T: ClassTag](data: Array[T]): ImmutableArray[T] = ImmutableArray[T](mutable.WrappedArray.make[T](data))

  def fill[T: ClassTag](n: Int)(elem: ⇒ T): ImmutableArray[T] = ImmutableArray(Array.fill[T](n)(elem))

  def empty[T: ClassTag]: ImmutableArray[T] = ImmutableArray(Array.empty[T])
}






case class UnzippedMap[A, B: ClassTag](keyArray: ImmutableArray[A], valueArray: ImmutableArray[B], filter: ImmutableArray[Boolean]) {

  def filterKeys(p: (A) ⇒ Boolean): UnzippedMap[A, B] = {
    val updatedFilter = (keyArray, filter).map((key, flag) => if (flag) p(key) else false)
    UnzippedMap(keyArray, valueArray, updatedFilter)
  }

  def filterOut(reject: ImmutableArray[Boolean]): UnzippedMap[A, B] = {
    val updatedFilter = (filter, reject).map((flag, reject) => if (flag) !reject else false)
    UnzippedMap(keyArray, valueArray, updatedFilter)
  }

  def filterOut(reject: (ArrayIndex) => Boolean): UnzippedMap[A, B] = {
    val updatedFilter = filter.mapWithIndex((flag, index) => if (flag) !reject(index) else false)
    UnzippedMap(keyArray, valueArray, updatedFilter)
  }


  def values: ImmutableArray[B] = {
    val b = Array.newBuilder[B]
    val len = keyArray.length

    var i = 0
    while (i < len) {
      if (filter(ArrayIndex(i))) b += valueArray(ArrayIndex(i))
      i += 1
    }

    new ImmutableArray(b.result)
  }

}

object UnzippedMap {

  def apply[A: ClassTag, B: ClassTag](keys: Iterable[A], values: Iterable[B]): UnzippedMap[A, B] = {
    UnzippedMap(ImmutableArray(keys.toArray[A]), ImmutableArray(values.toArray[B]), ImmutableArray.fill(keys.size)(true))
  }

}