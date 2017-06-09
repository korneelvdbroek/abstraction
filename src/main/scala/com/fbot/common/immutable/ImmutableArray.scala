package com.fbot.common.immutable

import scala.collection.mutable
import scala.math.Ordering
import scala.reflect.ClassTag

/**
  * Copyright (C) 6/3/2017 - REstore NV
  *
  */
/**
  * All methods which either change either
  * - the dimension of the result, or
  * - the type
  *
  * @tparam T
  */
trait ImmutableArrayOpsTransform[T, Self[T] <: ImmutableArrayOpsTransform[T, Self]] extends Any with ImmutableArrayOps[T, Self[T]] {

  def makeTransformed[B](x: mutable.WrappedArray[B]): Self[B]

  def make(x: mutable.WrappedArray[T]): Self[T] = makeTransformed(x)

  def ++(that: Self[T])(implicit evidence: scala.reflect.ClassTag[T]): Self[T] = {
    val thisLen = repr.toArray.length
    val thatLen = that.repr.toArray.length

    val x = new Array[T](thisLen + thatLen)
    System.arraycopy(repr.toArray, 0, x, 0, thisLen)
    System.arraycopy(that.repr.toArray, 0, x, thisLen, thatLen)
    make(x)
  }

  def map[B: ClassTag](f: (T) ⇒ B): Self[B] = makeTransformed(repr.map(f))

  def mapWithIndex[B: ClassTag](f: (T, ArrayIndex) ⇒ B): Self[B] = {
    val len = length
    val mapped = new Array[B](len)

    var i = 0
    while (i < len) {
      mapped(i) = f(repr(i), ArrayIndex(i))
      i += 1
    }

    makeTransformed(mapped)
  }

  def sortWith(lt: (T, T) ⇒ Boolean): Self[T] = make(repr.sortWith(lt))

  def sortBy[B](f: (T) ⇒ B)(implicit ord: math.Ordering[B]): Self[T] = make(repr.sortBy(f))

  def partialSort(k: Int, lt: (T, T) => Boolean)(implicit evidence: scala.reflect.ClassTag[T]): Self[T] = {

    val array = repr.toArray
    val ordering: Ordering[T] = Ordering fromLessThan lt
    val pq: mutable.PriorityQueue[T] = new mutable.PriorityQueue[T]()(ordering)

    // load up the PQ
    var i: Int = 0
    while (i < k && i < array.length) {
      pq.enqueue(array(i))
      i += 1
    }
    println(pq)

    //
    var j = k
    while (j < array.length) {
      println(s"${ array(j) } ${ ordering.compare(array(j), pq.head) }")
      if (ordering.compare(array(j), pq.head) <= 0) {
        pq.dequeue()
        pq.enqueue(array(j))
      }
      j += 1
    }

    make(pq.dequeueAll.toArray)
  }

  def take(k: Int)(implicit evidence: scala.reflect.ClassTag[T]): Self[T] = {
    val x = new Array[T](k)
    System.arraycopy(this.repr.toArray[T], 0, x, 0, k)
    make(x)
  }

  def flatten[U: ClassTag](implicit asArray: (T) ⇒ Self[U]): Self[U] = {
    val n = repr.map(elem => asArray(elem).length).sum
    val x = new Array[U](n)

    var i = 0
    repr foreach { elem =>
      val array = asArray(elem)
      System.arraycopy(array.repr.toArray[U], 0, x, i, array.length)
      i += array.length
    }

    makeTransformed(x)
  }

  def filter(p: (T) ⇒ Boolean): Self[T] = make(repr.filter(p))

  def filterNot(p: (T) ⇒ Boolean): Self[T] = make(repr.filterNot(p))

  def groupBy[K: ClassTag](f: (T) ⇒ K): Map[K, Self[T]] = {
    repr.groupBy(f).mapValues(make)
  }

  def unzippedGroupBy[K: ClassTag](f: (T) ⇒ K)(implicit evidence: scala.reflect.ClassTag[Self[T]]): UnzippedMap[K, Self[T]] = {
    UnzippedMap(repr.groupBy(f).mapValues(make))
  }

  def indexRange: Self[ArrayIndex] = makeTransformed(Array.range(0, repr.length).map(ArrayIndex(_)))

}




case class ImmutableArray[T](repr: mutable.WrappedArray[T]) extends AnyVal with ImmutableArrayOpsTransform[T, ImmutableArray] {

  def makeTransformed[B](x: mutable.WrappedArray[B]): ImmutableArray[B] = ImmutableArray(x)

}

object ImmutableArray {

  def apply[T: ClassTag](data: T*): ImmutableArray[T] = ImmutableArray[T](data.toArray)

  def apply[T: ClassTag](data: Array[T]): ImmutableArray[T] = ImmutableArray[T](mutable.WrappedArray.make[T](data))

  def fill[T: ClassTag](n: Int)(elem: ⇒ T): ImmutableArray[T] = ImmutableArray(Array.fill[T](n)(elem))

  def empty[T: ClassTag]: ImmutableArray[T] = ImmutableArray(Array.empty[T])

}