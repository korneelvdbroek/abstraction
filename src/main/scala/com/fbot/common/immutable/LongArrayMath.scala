package com.fbot.common.immutable

/**
  * Copyright (C) 6/8/2017 - REstore NV
  *
  */
object LongArrayMath {

  def zero(dim: Int): Array[Long] = Array.fill[Long](dim)(0L)

  def one(dim: Int): Array[Long] = Array.fill[Long](dim)(1L)

  def plus(a: Array[Long], b: Array[Long]): Array[Long] = elementWise(_ + _)(a, b)

  def minus(a: Array[Long], b: Array[Long]): Array[Long] = elementWise(_ - _)(a, b)

  def times(a: Array[Long], b: Array[Long]): Array[Long] = elementWise(_ * _)(a, b)

  def negate(a: Array[Long]): Array[Long] = {
    a.map(- _)
  }

  class Ops(lhs: Array[Long]) {
    def +(rhs: Array[Long]): Array[Long] = plus(lhs, rhs)
    def -(rhs: Array[Long]): Array[Long] = minus(lhs, rhs)
    def *(rhs: Array[Long]): Array[Long] = times(lhs, rhs)
    def unary_-(): Array[Long] = negate(lhs)
  }
  implicit def mkDoubleArrayMathOps(lhs: Array[Long]): Ops = new Ops(lhs)


  private def elementWise(f: (Long, Long) => Long)(a: Array[Long], b: Array[Long]): Array[Long] = {
    val c: Array[Long] = new Array[Long](a.length)
    var i: Int = 0
    while (i < a.length) {
      c(i) = f(a(i), b(i))
      i += 1
    }
    c
  }
}
