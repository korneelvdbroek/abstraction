package com.fbot.common.fastcollections.math

import com.fbot.common.fastcollections.FastTuple

import scala.collection.mutable

/**
  * Copyright (C) 6/3/2017 - REstore NV
  *
  */
trait FastArrayDoubleMath[Self <: FastTuple[Double, Self]] extends Any with ElementWiseFastArrayOps[Double] with FastTuple[Double, Self] {

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

  def unary_-(): Self = {
    make(this.repr.toArray.map(- _))
  }

}