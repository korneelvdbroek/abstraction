package com.fbot.common.immutable

/**
  * Copyright (C) 6/3/2017 - REstore NV
  *
  */
object DoubleArrayMath {
  def plus(a: Array[Double], b: Array[Double]): Array[Double] = elementWise(_ + _)(a, b)

  def minus(a: Array[Double], b: Array[Double]): Array[Double] = elementWise(_ - _)(a, b)

  def times(a: Array[Double], b: Array[Double]): Array[Double] = elementWise(_ * _)(a, b)

  def negate(a: Array[Double]): Array[Double] = {
    a.map(- _)
  }

  class Ops(lhs: Array[Double]) {
    def +(rhs: Array[Double]): Array[Double] = plus(lhs, rhs)
    def -(rhs: Array[Double]): Array[Double] = minus(lhs, rhs)
    def *(rhs: Array[Double]): Array[Double] = times(lhs, rhs)
    def unary_-(): Array[Double] = negate(lhs)
  }
  implicit def mkDoubleArrayMathOps(lhs: Array[Double]): Ops = new Ops(lhs)


  private def elementWise(f: (Double, Double) => Double)(a: Array[Double], b: Array[Double]): Array[Double] = {
    val c: Array[Double] = new Array[Double](a.length)
    var i: Int = 0
    while (i < a.length) {
      c(i) = f(a(i), b(i))
      i += 1
    }
    c
  }

}

