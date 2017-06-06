package com.fbot.common.immutable

/**
  * Copyright (C) 6/6/2017 - REstore NV
  *
  */
object BooleanArrayMath {
  def and(a: Array[Boolean], b: Array[Boolean]): Array[Boolean] = elementWise(_ && _)(a, b)

  def or(a: Array[Boolean], b: Array[Boolean]): Array[Boolean] = elementWise(_ || _)(a, b)

  def negate(a: Array[Boolean]): Array[Boolean] = {
    a.map(! _)
  }

  class Ops(lhs: Array[Boolean]) {
    def &&(rhs: Array[Boolean]): Array[Boolean] = and(lhs, rhs)
    def ||(rhs: Array[Boolean]): Array[Boolean] = or(lhs, rhs)
    def unary_!(): Array[Boolean] = negate(lhs)
  }
  implicit def mkBooleanArrayMathOps(lhs: Array[Boolean]): Ops = new Ops(lhs)


  private def elementWise(f: (Boolean, Boolean) => Boolean)(a: Array[Boolean], b: Array[Boolean]): Array[Boolean] = {
    val c: Array[Boolean] = new Array[Boolean](a.length)
    var i: Int = 0
    while (i < a.length) {
      c(i) = f(a(i), b(i))
      i += 1
    }
    c
  }
  
}
