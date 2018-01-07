package com.fbot.common.hyperspace

import com.fbot.common.fastcollections.math.FastTupleLongMath
import com.fbot.common.fastcollections.{BuilderFromArray, FastTuple, ImmutableArray}

import scala.collection.mutable
import scala.reflect.ClassTag


/**
  *
  */
case class HyperSpaceUnit(repr: mutable.WrappedArray[Long]) extends FastTuple[Long, HyperSpaceUnit] with FastTupleLongMath[HyperSpaceUnit] {

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

  implicit def builderFromArray[T](implicit m: ClassTag[T]): BuilderFromArray[Long, HyperSpaceUnit] = {
    new BuilderFromArray[Long, HyperSpaceUnit] {
      def result(array: Array[Long]): HyperSpaceUnit = HyperSpaceUnit(array)
    }
  }
}