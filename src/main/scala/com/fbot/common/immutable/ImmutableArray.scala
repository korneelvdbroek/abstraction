package com.fbot.common.immutable

/**
  * Copyright (C) 6/3/2017 - REstore NV
  *
  */
class ImmutableArray[T](val repr: Array[T]) extends AnyVal {

  def length: Int = repr.length

  def apply(index: ArrayIndex): T = repr(index.i)

  def map[B](f: (T) ⇒ B): ImmutableArray[B] = new ImmutableArray(repr.map(f))

  def sortWith(lt: (T, T) ⇒ Boolean): ImmutableArray[T] = new ImmutableArray(repr.sortWith(lt))

  def sortBy[B](f: (T) ⇒ B)(implicit ord: math.Ordering[B]): ImmutableArray[T] = new ImmutableArray(repr.sortBy(f))

  def take(n: Int): ImmutableArray[T] = new ImmutableArray(repr.take(n))

  def filterNot(p: (T) ⇒ Boolean): ImmutableArray[T] = new ImmutableArray(repr.filterNot(p))

  def groupBy[K](f: (T) ⇒ K): Map[K, ImmutableArray[T]] = repr.groupBy(f).mapValues(new ImmutableArray(_))


  override def toString: String = repr.mkString("[", ",\n", "]")

  def indexRange: ImmutableArray[ArrayIndex] = new ImmutableArray(Array.range(0, repr.length).map(ArrayIndex(_)))

}


object ImmutableArray {

  def apply[T](data: Array[T]): ImmutableArray[T] = new ImmutableArray[T](data)

  def apply[T](data: T*): ImmutableArray[T] = new ImmutableArray[T](data.toArray)

}