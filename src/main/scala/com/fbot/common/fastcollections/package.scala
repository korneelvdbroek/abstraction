package com.fbot.common

import com.fbot.common.fastcollections.fastarrayops._
import shapeless.newtype
import shapeless.newtype.Newtype

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
  * Value classes in scala don't really do their job, so we need tagged types
  * https://failex.blogspot.be/2017/04/the-high-cost-of-anyval-subclasses.html
  * We use shapeless since it makes T = Int translucent (scalaz7+ does not),
  * hence the type erasure of the tagged type is the primitive type.
  *
  * specialized does not really do its job in scala, it does not specialize an Array[T] contained in a trait with type T specialized
  * http://www.scala-lang.org/old/node/10408.html
  */
package object fastcollections {

  /**
    * ArrayIndex
    */
  type ArrayIndex = Newtype[Int, ArrayIndexOps]

  def ArrayIndex(i: Int): ArrayIndex = newtype[Int, ArrayIndexOps](i)

  case class ArrayIndexOps(i: Int) {

    def toInt: Int = i

    def +=(j: Int): ArrayIndex = ArrayIndex(i + j)

    def <(j: Int): Boolean = i < j
    def ==(j: Int): Boolean = i == j
    def >(j: Int): Boolean = i > j
  }

  implicit def arrayIndexOps(index: ArrayIndex): ArrayIndexOps = ArrayIndexOps(index.asInstanceOf[Int])


  /**
    * Tuple
    */
  type Tuple = Newtype[Array[Double], TupleOps]

  def Tuple(arr: Array[Double]): Tuple = newtype[Array[Double], TupleOps](arr)

  def Tuple(d: Double*): Tuple = Tuple(d.toArray)

  implicit def tupleOps(t: Tuple): TupleOps = TupleOps(t.asInstanceOf[Array[Double]])

  object Tuple {

    def fill(dim: Int)(elem: ⇒ Double): Tuple = Tuple(Array.fill(dim)(elem))
  }

  /**
    * ImmutableArray
    */
  type ImmutableArray[T] = Newtype[Array[T], FastArrayOps]

  def ImmutableArray[T](arr: Array[T]): ImmutableArray[T] = newtype(arr)

  implicit def immutableArrayOps4Generic[T](t: ImmutableArray[T]): FastGenericArrayOps[T] = FastGenericArrayOps(t.asInstanceOf[Array[T]])

  implicit def immutableArrayOps4Double(t: ImmutableArray[Double]): FastDoubleArrayOps = FastDoubleArrayOps(t.asInstanceOf[Array[Double]])

  implicit def immutableArrayOps4Int(t: ImmutableArray[Int]): FastIntArrayOps = FastIntArrayOps(t.asInstanceOf[Array[Int]])

  implicit def immutableArrayOps4Long(t: ImmutableArray[Long]): FastLongArrayOps = FastLongArrayOps(t.asInstanceOf[Array[Long]])

  object ImmutableArray {

    def empty[@specialized(Double, Int, Long) A]: ImmutableArray[A] = ImmutableArray(new Array[A](0))

    def indexRange(length: Int): ImmutableArray[Int] = ImmutableArray(Array.range(0, length))

    def indexRange(length: Long): ImmutableArray[Long] = ImmutableArray(Array.range(0, length.toInt).map(_.toLong))

    def range(from: Int, to: Int): ImmutableArray[Int] = ImmutableArray(Array.range(from, to))

    def range(from: Long, to: Long): ImmutableArray[Long] = ImmutableArray(Array.range(from.toInt, to.toInt).map(_.toLong))

  }

}
