package com.fbot.common.immutable

import scala.reflect.ClassTag
import BooleanArrayMath._
import ZippedImmutableArray2._


/**
  * Copyright (C) 6/3/2017 - REstore NV
  *
  */
class ImmutableArray[T](val repr: Array[T]) extends AnyVal {

  def ++(that: ImmutableArray[T]): ImmutableArray[T] = {
    val x = new Array[T](this.repr.length + that.repr.length)
    System.arraycopy(this.repr, 0, x, 0, this.repr.length)
    System.arraycopy(that.repr, 0, x, this.repr.length, that.repr.length)
    new ImmutableArray(x)
  }

  def length: Int = repr.length

  def apply(index: ArrayIndex): T = repr(index.i)

  def last: T = repr(length - 1)

  def map[B: ClassTag](f: (T) ⇒ B): ImmutableArray[B] = new ImmutableArray(repr.map(f))

  def sortWith(lt: (T, T) ⇒ Boolean): ImmutableArray[T] = new ImmutableArray(repr.sortWith(lt))

  def sortBy[B](f: (T) ⇒ B)(implicit ord: math.Ordering[B]): ImmutableArray[T] = new ImmutableArray(repr.sortBy(f))

  def take(n: Int): ImmutableArray[T] = {
    val x = new Array[T](n)
    System.arraycopy(this.repr, 0, x, 0, n)
    new ImmutableArray(x)
  }

  def flatten[U](implicit asArray: (T) ⇒ ImmutableArray[U], m: ClassTag[U]): ImmutableArray[U] = {
    new ImmutableArray(repr.flatten(asArray(_).toList, m))
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

  def apply[T](data: Array[T]): ImmutableArray[T] = ImmutableArray[T](data)

  def apply[T: ClassTag](data: T*): ImmutableArray[T] = ImmutableArray[T](data.toArray)

  def fill[T: ClassTag](n: Int)(elem: ⇒ T): ImmutableArray[T] = ImmutableArray(Array.fill[T](n)(elem))

  def empty[T]: ImmutableArray[T] = ImmutableArray(Array.empty[T])
}






case class UnzippedMap[A, B](keyArray: ImmutableArray[A], valueArray: ImmutableArray[B], filter: ImmutableArray[Boolean]) {

  def filterKeys(p: (A) ⇒ Boolean): UnzippedMap[A, B] = {
    val updatedFilter = (keyArray, filter).map((key, flag) => if (flag) p(key) else false)
    new UnzippedMap(keyArray, valueArray, updatedFilter)
  }

  def filterOut(reject: ImmutableArray[Boolean]): UnzippedMap[A, B] = {
    val updatedFilter = (filter, reject).map((flag, reject) => if (flag) !reject else false)
    new UnzippedMap(keyArray, valueArray, updatedFilter)
  }

  def values: ImmutableArray[B] = {
    val b = Array.newBuilder[B]
    val len = keyArray.length

    var i = 0
    while (i < len) {
      if (filter(i)) b += valueArray(i)
      i += 1
    }

    new ImmutableArray(b.result)
  }

}

object UnzippedMap {

  def apply[A: ClassTag, B: ClassTag](keys: Iterable[A], values: Iterable[B]): UnzippedMap[A, B] = {
    new UnzippedMap(new ImmutableArray(keys.toArray[A]), new ImmutableArray(values.toArray[B]), ImmutableArray.fill(keys.size)(true))
  }

}




case class UnzippedMapFilter(repr: Array[Boolean]) extends AnyVal

object UnzippedMapFilter {

  implicit def fromImmutableArray(filter: ImmutableArray[Boolean]): UnzippedMapFilter = new UnzippedMapFilter(filter.repr)

  def and(a: UnzippedMapFilter, b: UnzippedMapFilter): UnzippedMapFilter = UnzippedMapFilter(a.repr && b.repr)

  def or(a: UnzippedMapFilter, b: UnzippedMapFilter): UnzippedMapFilter = UnzippedMapFilter(a.repr || b.repr)

  def negate(a: UnzippedMapFilter): UnzippedMapFilter = UnzippedMapFilter(!a.repr)

  class Ops(lhs: UnzippedMapFilter) {
    def &&(rhs: UnzippedMapFilter): UnzippedMapFilter = and(lhs, rhs)
    def ||(rhs: UnzippedMapFilter): UnzippedMapFilter = or(lhs, rhs)
    def unary_!(): UnzippedMapFilter = negate(lhs)
  }
  implicit def mkUnzippedMapFilterOps(lhs: UnzippedMapFilter): Ops = new Ops(lhs)
}