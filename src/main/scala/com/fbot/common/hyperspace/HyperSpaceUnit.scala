package com.fbot.common.hyperspace

import com.fbot.common.fastcollections.math.FastArrayLongMath
import com.fbot.common.fastcollections.{FastTuple, ImmutableArray}

import scala.collection.mutable


/**
  *
  */
case class HyperSpaceUnit(repr: mutable.WrappedArray[Long]) extends AnyVal with FastTuple[Long, HyperSpaceUnit] with FastArrayLongMath[HyperSpaceUnit] {

  def make(x: mutable.WrappedArray[Long]): HyperSpaceUnit = HyperSpaceUnit(x)

  def project(space: HyperSpace): HyperSpaceUnit = {
    HyperSpaceUnit(space.embeddingAxes.map(embeddingAxis => repr(embeddingAxis.toInt)).toArray)
  }

  def dim: Int = repr.length

  override def toString: String = {
    repr.repr.mkString("Cube(", ",", ")")
  }

}

object HyperSpaceUnit {

  def apply(position: Long*): HyperSpaceUnit = HyperSpaceUnit(position.toArray)

  def unit(dim: Int): HyperSpaceUnit = {
    HyperSpaceUnit(Array.fill[Long](dim)(1L))
  }

}