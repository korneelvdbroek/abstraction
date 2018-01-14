package com.fbot.common.fastcollections.deprecated.math

import com.fbot.common.fastcollections.deprecated.FastTuple

import scala.collection.mutable
import scala.reflect.ClassTag

/**
  *
  */
trait FastTupleDoubleMath[Self <: FastTuple[Double, Self]] extends Any with ElementWiseFastArrayOps[Double] with FastTuple[Double, Self] {

  def make(x: mutable.WrappedArray[Double]): Self

  private def make(x: Array[Double]): Self = {
    // avoid the match case, and go straight to Double
    make(new mutable.WrappedArray.ofDouble(x).asInstanceOf[mutable.WrappedArray[Double]])
  }


  def +(rhs: Self): Self = {
    val res: Array[Double] = new Array[Double](this.length)
    make(elementWise(_ + _)(this.repr.toArray, rhs.repr.toArray)(res))
  }

  def -(rhs: Self): Self = {
    val res: Array[Double] = new Array[Double](this.length)
    make(elementWise(_ - _)(this.repr.toArray, rhs.repr.toArray)(res))
  }

  def *(rhs: Self): Self = {
    val res: Array[Double] = new Array[Double](this.length)
    make(elementWise(_ * _)(this.repr.toArray, rhs.repr.toArray)(res))
  }

  def /(rhs: Self): Self = {
    val res: Array[Double] = new Array[Double](this.length)
    make(elementWise(_ / _)(this.repr.toArray, rhs.repr.toArray)(res))
  }

  def unary_-(): Self = {
    make(this.repr.toArray.map(- _))
  }

  def *[B <: FastTuple[Long, B]: ClassTag](rhs: B): Self = {
    val res: Array[Double] = new Array[Double](this.length)
    make(elementWise(_ * _)(this.repr.toArray, rhs.repr.map(_.toDouble).toArray)(res))
  }

}