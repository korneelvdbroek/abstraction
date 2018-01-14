package com.fbot.common.fastcollections.deprecated.math

import com.fbot.common.fastcollections.ImmutableArray
import com.fbot.common.fastcollections.deprecated.FastTuple

import scala.collection.mutable

/**
  *
  */
trait FastTupleLongMath[Self <: FastTuple[Long, Self]] extends ElementWiseFastArrayOps[Long] with FastTuple[Long, Self] {

  def make(x: mutable.WrappedArray[Long]): Self

  private def make(x: Array[Long]): Self = {
    // avoid the match case, and go straight to Long
    make(new mutable.WrappedArray.ofLong(x).asInstanceOf[mutable.WrappedArray[Long]])
  }


  def +(rhs: Self): Self = {
    val res: Array[Long] = new Array[Long](this.length)
    make(elementWise(_ + _)(this.repr.toArray, rhs.repr.toArray)(res))
  }

  def -(rhs: Self): Self = {
    val res: Array[Long] = new Array[Long](this.length)
    make(elementWise(_ - _)(this.repr.toArray, rhs.repr.toArray)(res))
  }

  def *(rhs: Self): Self = {
    val res: Array[Long] = new Array[Long](this.length)
    make(elementWise(_ * _)(this.repr.toArray, rhs.repr.toArray)(res))
  }

  def unary_-(): Self = {
    make(this.repr.toArray.map(- _))
  }

}

