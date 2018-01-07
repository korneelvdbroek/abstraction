package com.fbot.common.fastcollections

import scala.collection.mutable
import scala.math.Ordering
import scala.reflect.ClassTag
import scala.runtime.ScalaRunTime.arrayElementClass

/**
  * All methods which either change either
  * - the dimension of the result, or
  * - the type
  *
  * @tparam A    the collection element type.
  * @tparam Repr the actual type of the element container.
  *
  */
trait FastArray[A, Repr] extends FastTuple[A, Repr] {

  def map[B: ClassTag, To](f: (A) ⇒ B)(implicit builder: BuilderFromArray[B, To]): To = builder.result(repr.map(f).toArray)

  def mapWithIndex[B: ClassTag, To](f: (A, Int) ⇒ B)(implicit builder: BuilderFromArray[B, To]): To = {
    val len = length
    val mapped = new Array[B](len)

    var i = 0
    while (i < len) {
      mapped(i) = f(repr(i), i)
      i += 1
    }

    builder.result(mapped)
  }

  def flatMap[B: ClassTag, To](f: A => FastArray[B, To])(implicit builder: BuilderFromArray[B, To]): To = {
    builder.result(repr.flatMap(element => f(element).repr).toArray)
  }

  def flatten[B: ClassTag, To](implicit builder: BuilderFromArray[B, To], asArray: (A) ⇒ FastArray[B, To]): To = {
    val n = repr.map(elem => asArray(elem).length).sum
    val x = new Array[B](n)

    var i = 0
    repr foreach { elem =>
      val array = asArray(elem)
      System.arraycopy(array.repr.toArray, 0, x, i, array.length)
      i += array.length
    }

    builder.result(x)
  }


  def sortWith(lt: (A, A) ⇒ Boolean)(implicit evidence: scala.reflect.ClassTag[A], builder: BuilderFromArray[A, Repr]): Repr = builder
    .result(repr.sortWith(lt).toArray)

  def sortBy[B](f: (A) ⇒ B)(implicit evidence: scala.reflect.ClassTag[A], ord: Ordering[B], builder: BuilderFromArray[A, Repr]): Repr = builder
    .result(repr.sortBy(f).toArray)

  def partialSort(k: Int, lt: (A, A) => Boolean)(implicit evidence: scala.reflect.ClassTag[A], builder: BuilderFromArray[A, Repr]): Repr = {
    val array = repr.toArray
    val ordering: Ordering[A] = Ordering fromLessThan lt
    val pq: mutable.PriorityQueue[A] = new mutable.PriorityQueue[A]()(ordering)

    // load up the PQ
    var i: Int = 0
    while (i < k && i < array.length) {
      pq.enqueue(array(i))
      i += 1
    }

    // evaluate rest of array
    while (i < array.length) {
      //println(s"${ array(i) } ${ ordering.compare(array(i), pq.head) }")
      if (ordering.compare(array(i), pq.head) <= 0) {
        pq.dequeue()
        pq.enqueue(array(i))
      }
      i += 1
    }

    builder.result(pq.dequeueAll.reverse.toArray)
  }

  def take(k: Int)(implicit evidence: scala.reflect.ClassTag[A], builder: BuilderFromArray[A, Repr]): Repr = {
    val x = new Array[A](k)
    System.arraycopy(this.repr.toArray[A], 0, x, 0, k)
    builder.result(x)
  }

  def slice(from: Int, until: Int)(implicit evidence: scala.reflect.ClassTag[A], builder: BuilderFromArray[A, Repr]): Repr = builder
    .result(repr.slice(from, until).toArray)

  def filter(p: (A) ⇒ Boolean)(implicit evidence: scala.reflect.ClassTag[A], builder: BuilderFromArray[A, Repr]): Repr = builder.result(repr.filter(p).toArray)

  def filterByIndex(p: Int => Boolean)(implicit evidence: scala.reflect.ClassTag[A], builder: BuilderFromArray[A, Repr]): Repr = {
    val len = length
    val filtered = new mutable.WrappedArrayBuilder[A](ClassTag[A](arrayElementClass(repr.getClass)))

    var i = 0
    while (i < len) {
      if (p(i)) filtered += repr(i)
      i += 1
    }

    builder.result(filtered.result.toArray)
  }


  def filterNot(p: (A) ⇒ Boolean)(implicit evidence: scala.reflect.ClassTag[A], builder: BuilderFromArray[A, Repr]): Repr = builder
    .result(repr.filterNot(p).toArray)

  def groupBy[Key: ClassTag](f: (A) ⇒ Key)(implicit evidence: scala.reflect.ClassTag[A], builder: BuilderFromArray[A, Repr]): Map[Key, Repr] = {
    repr.groupBy(f).mapValues(array => builder.result(array.toArray))
  }

  def indexRange[To](implicit builder: BuilderFromArray[Int, To]): To = builder.result(Array.range(0, repr.length))

  def sum[B >: A](implicit num: Numeric[B]): B = repr.sum(num)

  def max[B >: A](implicit cmp: Ordering[B]): A = repr.max(cmp)

}


