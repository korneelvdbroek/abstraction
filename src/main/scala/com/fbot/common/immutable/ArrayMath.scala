package com.fbot.common.immutable

import scala.reflect.ClassTag

/**
  * Copyright (C) 6/3/2017 - REstore NV
  *
  */
trait ArrayMath[T] {

  def elementWise(f: (T, T) => T)(a: Array[T], b: Array[T])(implicit m: ClassTag[T]): Array[T] = {
    val c: Array[T] = new Array[T](a.length)
    var i: Int = 0
    while (i < a.length) {
      c(i) = f(a(i), b(i))
      i += 1
    }
    c
  }

  def plus(a: Array[T], b: Array[T])(implicit num: Numeric[T], m: ClassTag[T]): Array[T] = elementWise(num.plus)(a, b)

  def minus(a: Array[T], b: Array[T])(implicit num: Numeric[T], m: ClassTag[T]): Array[T] = elementWise(num.minus)(a, b)

  def times(a: Array[T], b: Array[T])(implicit num: Numeric[T], m: ClassTag[T]): Array[T] = elementWise(num.times)(a, b)

  def negate(a: Array[T])(implicit num: Numeric[T], m: ClassTag[T]): Array[T] = {
    a.map(num.negate)
  }

  class Ops(lhs: Array[T])(implicit num: Numeric[T], m: ClassTag[T]) {
    def +(rhs: Array[T]): Array[T] = plus(lhs, rhs)
    def -(rhs: Array[T]): Array[T] = minus(lhs, rhs)
    def *(rhs: Array[T]): Array[T] = times(lhs, rhs)
    def unary_-(): Array[T] = negate(lhs)
  }
  implicit def mkArrayMathOps(lhs: Array[T])(implicit num: Numeric[T], m: ClassTag[T]): Ops = new Ops(lhs)

}

object DoubleArrayMath extends ArrayMath[Double]

