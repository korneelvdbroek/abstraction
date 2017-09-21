package com.fbot.common.hyperspace

import com.fbot.common.fastcollections.math.FastTupleLongMath
import com.fbot.common.fastcollections.{FastTuple, ImmutableArray}

import scala.collection.mutable


/**
  *
  */
case class HyperSpaceUnit(repr: mutable.WrappedArray[Long]) extends AnyVal with FastTuple[Long, HyperSpaceUnit] with FastTupleLongMath[HyperSpaceUnit] {

  def make(x: mutable.WrappedArray[Long]): HyperSpaceUnit = HyperSpaceUnit(x)

  def dim: Int = repr.length

  override def toString: String = {
    mkString("SpaceUnit(", ",", ")")
  }

  def mkString(space: HyperSpace): String = {
    space.toCoordinate(this).mkString("SpaceUnit(", ",", ")")
  }

}

object HyperSpaceUnit {

  def apply(position: Long*): HyperSpaceUnit = HyperSpaceUnit(position.toArray)

  def unit(dim: Int): HyperSpaceUnit = {
    HyperSpaceUnit(Array.fill[Long](dim)(1L))
  }

  implicit def fromImmutableArray(array: ImmutableArray[Long]): HyperSpaceUnit = HyperSpaceUnit(array.repr)

}