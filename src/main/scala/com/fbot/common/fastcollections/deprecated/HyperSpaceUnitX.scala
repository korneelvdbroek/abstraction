package com.fbot.common.fastcollections.deprecated

import com.fbot.common.fastcollections.deprecated.math.FastTupleLongMath
import com.fbot.common.hyperspace.HyperSpace

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
case class HyperSpaceUnitX(repr: mutable.WrappedArray[Long]) extends FastTuple[Long, HyperSpaceUnitX] with FastTupleLongMath[HyperSpaceUnitX] {

  def make(x: mutable.WrappedArray[Long]): HyperSpaceUnitX = HyperSpaceUnitX(x)

  def dim: Int = repr.length

  override def toString: String = {
    mkString("SpaceUnit(", ",", ")")
  }

//  def mkString(space: HyperSpace): String = {
//    space.toCoordinate(this).mkString("SpaceUnit(", ",", ")")
//  }

}

object HyperSpaceUnitX {

  def apply(position: Long*): HyperSpaceUnitX = HyperSpaceUnitX(position.toArray)

  def unit(dim: Int): HyperSpaceUnitX = {
    HyperSpaceUnitX(Array.fill[Long](dim)(1L))
  }

//  implicit def fromImmutableArray(array: ImmutableArray[Long]): HyperSpaceUnitX = HyperSpaceUnitX(array.repr)

  implicit def builderFromArray[T](implicit m: ClassTag[T]): BuilderFromArray[Long, HyperSpaceUnitX] = {
    new BuilderFromArray[Long, HyperSpaceUnitX] {
      def result(array: Array[Long]): HyperSpaceUnitX = HyperSpaceUnitX(array)
    }
  }
}