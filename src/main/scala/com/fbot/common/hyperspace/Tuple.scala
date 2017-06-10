package com.fbot.common.hyperspace

import com.fbot.common.fastcollections.FastTuple
import com.fbot.common.fastcollections.math.FastArrayDoubleMath

import scala.collection.mutable

/**
  * Copyright (C) 5/30/2017 - REstore NV
  *
  */
case class Tuple(repr: mutable.WrappedArray[Double]) extends AnyVal with FastTuple[Double, Tuple] with FastArrayDoubleMath[Tuple] {

  def make(x: mutable.WrappedArray[Double]): Tuple = Tuple(x)

  def dim: Int = repr.length

  def apply(i: Int): Double = repr(i)

  override def toString: String = {
    repr.mkString("(", ", ", ")")
  }

}

object Tuple {

  def apply(data: Double*): Tuple = new Tuple(data.toArray)

}


