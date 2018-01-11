package com.fbot.common.hyperspace

import com.fbot.common.fastcollections.{BuilderFromArray, FastTuple, ImmutableArray}
import com.fbot.common.fastcollections.math.FastTupleDoubleMath

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
  *
  * Tuple is a value class:
  * - Tuples are usually smaller and occur often, so we don't want boxing overhead
  * - since it is a value class we loose the advantage of specializing, but that's ok since Tuple is defined on Double only!
  *
  * Value classes are trouble for runtime performance: https://failex.blogspot.be/2017/04/the-high-cost-of-anyval-subclasses.html
  */
case class TupleX(repr: mutable.WrappedArray[Double]) extends FastTuple[Double, TupleX] with FastTupleDoubleMath[TupleX] {

  // needed for FastTupleDoubleMath
  def make(x: mutable.WrappedArray[Double]): TupleX = TupleX(x)

  def dim: Int = repr.length

  def slice(from: Int, until: Int): TupleX = TupleX(repr.slice(from, until))

  override def toString: String = {
    repr.mkString("(", ", ", ")")
  }

}

object TupleX {

  def apply(data: Double*): TupleX = new TupleX(data.toArray)

  def apply(data: TraversableOnce[Double]): TupleX = TupleX(data.toArray)

  def fill(dim: Int)(elem: ⇒ Double): TupleX = TupleX(Array.fill(dim)(elem))

  implicit def fromImmutableArray(array: ImmutableArray[Double]): TupleX = TupleX(array.repr)

  implicit def builderFromArray[T](implicit m: ClassTag[T]): BuilderFromArray[Double, TupleX] = {
    new BuilderFromArray[Double, TupleX] {
      def result(array: Array[Double]): TupleX = TupleX(array)
    }
  }

}


