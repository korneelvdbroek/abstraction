package com.fbot.algos.mutualinformation

import com.fbot.common.immutable.DoubleArrayMath._
import com.fbot.common.immutable.ImmutableArrayOps

import scala.collection.mutable

/**
  * Copyright (C) 5/30/2017 - REstore NV
  *
  */
case class Tuple(repr: mutable.WrappedArray[Double]) extends AnyVal with ImmutableArrayOps[Double, Tuple] {

  def make(x: mutable.WrappedArray[Double]): Tuple = Tuple(x)

  def dim: Int = repr.length

  def apply(i: Int): Double = repr(i)

  override def toString: String = {
    repr.mkString("(", ", ", ")")
  }

}

object Tuple {

  def apply(data: Double*): Tuple = new Tuple(data.toArray)

  def zero(dim: Int): Tuple = Tuple(Array.fill(dim)(0d))

  def one(dim: Int): Tuple = Tuple(Array.fill(dim)(1d))

  def plus(a: Tuple, b: Tuple): Tuple = Tuple(a.repr.toArray + b.repr.toArray)

  def minus(a: Tuple, b: Tuple): Tuple = Tuple(a.repr.toArray - b.repr.toArray)

  def times(a: Tuple, b: Tuple): Tuple = Tuple(a.repr.toArray * b.repr.toArray)

  def negate(a: Tuple): Tuple = Tuple(-a.repr.toArray)

  class Ops(lhs: Tuple) {
    def +(rhs: Tuple): Tuple = plus(lhs, rhs)
    def -(rhs: Tuple): Tuple = minus(lhs, rhs)
    def *(rhs: Tuple): Tuple = times(lhs, rhs)
    def unary_-(): Tuple = negate(lhs)
  }
  implicit def mkTupleOps(lhs: Tuple): Ops = new Ops(lhs)

}


