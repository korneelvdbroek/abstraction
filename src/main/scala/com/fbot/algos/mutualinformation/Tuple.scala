package com.fbot.algos.mutualinformation

import com.fbot.common.immutable.DoubleArrayMath._

/**
  * Copyright (C) 5/30/2017 - REstore NV
  *
  */
case class Tuple(data: Array[Double]) {

  def dim: Int = data.length

  def apply(i: Int): Double = data(i)

  override def toString: String = {
    data.mkString("(", ", ", ")")
  }

}

object Tuple {

  def apply(data: Double*): Tuple = new Tuple(data.toArray)

  implicit def tupleToArray(tuple: Tuple): Array[Double] = tuple.data

  def zero(dim: Int): Tuple = Tuple(Array.fill(dim)(0d))

  def one(dim: Int): Tuple = Tuple(Array.fill(dim)(1d))

  def plus(a: Tuple, b: Tuple): Tuple = Tuple(a.data + b.data)

  def minus(a: Tuple, b: Tuple): Tuple = Tuple(a.data - b.data)

  def times(a: Tuple, b: Tuple): Tuple = Tuple(a.data * b.data)

  def negate(a: Tuple): Tuple = Tuple(-a.data)

  class Ops(lhs: Tuple) {
    def +(rhs: Tuple): Tuple = plus(lhs, rhs)
    def -(rhs: Tuple): Tuple = minus(lhs, rhs)
    def *(rhs: Tuple): Tuple = times(lhs, rhs)
    def unary_-(): Tuple = negate(lhs)
  }
  implicit def mkTupleOps(lhs: Tuple): Ops = new Ops(lhs)

}


