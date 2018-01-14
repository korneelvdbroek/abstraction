package com.fbot.common.hyperspace

import com.fbot.common.fastcollections.deprecated.{BuilderFromArray, FastTuple}
import com.fbot.common.fastcollections.deprecated.math.FastTupleLongMath
import com.fbot.common.fastcollections.{BuilderFromArray, ImmutableArray$}

import scala.collection.mutable
import scala.reflect.ClassTag


/**
  * Copyright (C) 2017-2018  korneelvdbroek
  *
  * This program is free software: you can redistribute it and/or modify
  * it under the terms of the GNU General Public License as published by
  * the Free Software Foundation, either version 3 of the License, or
  * (at your option) any later version.
  *
  * This program is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU General Public License for more details.
  *
  * You should have received a copy of the GNU General Public License
  * along with this program.  If not, see <http://www.gnu.org/licenses/>.
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