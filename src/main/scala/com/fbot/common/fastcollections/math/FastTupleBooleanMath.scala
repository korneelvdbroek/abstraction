package com.fbot.common.fastcollections.math

import com.fbot.common.fastcollections.FastTuple

import scala.collection.mutable

/**
  *
  */
trait FastTupleBooleanMath[Self <: FastTuple[Boolean, Self]] extends Any with ElementWiseFastArrayOps[Boolean] with FastTuple[Boolean, Self] {

  def make(x: mutable.WrappedArray[Boolean]): Self

  private def make(x: Array[Boolean]): Self = {
    // avoid the match case, and go straight to Boolean
    make(new mutable.WrappedArray.ofBoolean(x).asInstanceOf[mutable.WrappedArray[Boolean]])
  }


  def &&(rhs: Self): Self = {
    val res: Array[Boolean] = new Array[Boolean](this.length)
    make(elementWise(_ && _)(this.repr.toArray, rhs.repr.toArray)(res))
  }

  def ||(rhs: Self): Self = {
    val res: Array[Boolean] = new Array[Boolean](this.length)
    make(elementWise(_ || _)(this.repr.toArray, rhs.repr.toArray)(res))
  }

  def unary_!(): Self = {
    make(this.repr.toArray.map(! _))
  }

}
