package com.fbot.common.immutable

import scala.collection.mutable

/**
  * Copyright (C) 6/3/2017 - REstore NV
  *
  */
trait DoubleImmutableArrayMath[Self <: ImmutableArrayOps[Double, Self]] extends Any with ElementWiseArrayOps[Double] {

  def make(x: mutable.WrappedArray[Double]): Self

  def make(x: Array[Double]): Self = {
    // avoid the match case, and go straight to Double
    make(new mutable.WrappedArray.ofDouble(x).asInstanceOf[mutable.WrappedArray[Double]])
  }

  def plus(a: ImmutableArrayOps[Double, Self], b: ImmutableArrayOps[Double, Self]): Self = {
    val res: Array[Double] = new Array[Double](a.length)
    make(elementWise(_ + _)(a.repr.toArray, b.repr.toArray)(res))
  }

  def minus(a: ImmutableArrayOps[Double, Self], b: ImmutableArrayOps[Double, Self]): Self = {
    val res: Array[Double] = new Array[Double](a.length)
    make(elementWise(_ - _)(a.repr.toArray, b.repr.toArray)(res))
  }

  def times(a: ImmutableArrayOps[Double, Self], b: ImmutableArrayOps[Double, Self]): Self = {
    val res: Array[Double] = new Array[Double](a.length)
    make(elementWise(_ * _)(a.repr.toArray, b.repr.toArray)(res))
  }

  def negate(a: ImmutableArrayOps[Double, Self]): Self = {
    make(a.repr.toArray.map(- _))
  }

  class Ops(lhs: ImmutableArrayOps[Double, Self]) {
    def +(rhs: ImmutableArrayOps[Double, Self]): Self = plus(lhs, rhs)
    def -(rhs: ImmutableArrayOps[Double, Self]): Self = minus(lhs, rhs)
    def *(rhs: ImmutableArrayOps[Double, Self]): Self = times(lhs, rhs)
    def unary_-(): Self = negate(lhs)
  }
  implicit def mkDoubleArrayMathOps(lhs: ImmutableArrayOps[Double, Self]): Ops = new Ops(lhs)

}


trait ElementWiseArrayOps[@specialized T] extends Any {

  /**
    *
    * @param f    element wise operation
    * @param a    array with first elements
    * @param b    array with second elements
    * @param res  array with results
    *             note: needs to be created outside this function, since Array[T] creating requires ClassTag which defeats specialization
    * @return     array with results
    */
  protected def elementWise(f: (T, T) => T)(a: Array[T], b: Array[T])(res: Array[T]): Array[T] = {
    var i: Int = 0
    while (i < a.length) {
      res(i) = f(a(i), b(i))
      i += 1
    }
    res
  }

}
