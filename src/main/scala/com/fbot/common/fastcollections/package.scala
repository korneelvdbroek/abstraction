package com.fbot.common

import com.fbot.common.fastcollections.fastarrayops._
import com.fbot.common.fastcollections.tupleops.{HyperSpaceUnitOps, TupleOps}
import shapeless.newtype
import shapeless.newtype.Newtype

import scala.io.Source
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
    * HyperSpaceUnit
    */
  type HyperSpaceUnit = Newtype[Array[Long], HyperSpaceUnitOps]

  def HyperSpaceUnit(arr: Array[Long]): HyperSpaceUnit = newtype[Array[Long], HyperSpaceUnitOps](arr)

  def HyperSpaceUnit(d: Long*): HyperSpaceUnit = HyperSpaceUnit(d.toArray)

  implicit def hyperSpaceUnitOps(t: HyperSpaceUnit): HyperSpaceUnitOps = HyperSpaceUnitOps(t.asInstanceOf[Array[Long]])

  object HyperSpaceUnit {

    def fill(dim: Int)(elem: ⇒ Long): HyperSpaceUnit = HyperSpaceUnit(Array.fill(dim)(elem))

    def apply(position: Long*): HyperSpaceUnit = HyperSpaceUnit(position.toArray)

    def unit(dim: Int): HyperSpaceUnit = HyperSpaceUnit(Array.fill[Long](dim)(1L))
  }


  /**
    * ImmutableArray
    */
  type ImmutableArray[T] = Newtype[Array[T], FastArrayOps]

  def ImmutableArray[@specialized(Double, Int, Long) T](arr: Array[T]): ImmutableArray[T] = newtype(arr)

  implicit def immutableArrayOps4Generic[T](t: ImmutableArray[T]): FastGenericArrayOps[T] = FastGenericArrayOps(t.asInstanceOf[Array[T]])

  implicit def immutableArrayOps4Double(t: ImmutableArray[Double]): FastDoubleArrayOps = FastDoubleArrayOps(t.asInstanceOf[Array[Double]])

  implicit def immutableArrayOps4Int(t: ImmutableArray[Int]): FastIntArrayOps = FastIntArrayOps(t.asInstanceOf[Array[Int]])

  implicit def immutableArrayOps4Long(t: ImmutableArray[Long]): FastLongArrayOps = FastLongArrayOps(t.asInstanceOf[Array[Long]])

  object ImmutableArray {
    def apply[@specialized(Double, Int, Long) A: ClassTag](data0: A, dataRest: A*): ImmutableArray[A] = ImmutableArray[A]((data0 +: dataRest).toArray)

    def apply[@specialized(Double, Int, Long) A: ClassTag](data: TraversableOnce[A]): ImmutableArray[A] = ImmutableArray[A](data.toArray)

    def fill[@specialized(Double, Int, Long) A: ClassTag](n: Int)(elem: ⇒ A): ImmutableArray[A] = ImmutableArray(Array.fill[A](n)(elem))

    def empty[@specialized(Double, Int, Long) A: ClassTag]: ImmutableArray[A] = ImmutableArray(new Array[A](0))

    def indexRange(length: Int): ImmutableArray[ArrayIndex] = ImmutableArray(Array.range(0, length).map(ArrayIndex))

    def range(from: Int, to: Int): ImmutableArray[Int] = ImmutableArray(Array.range(from, to))

    def range(from: Long, to: Long): ImmutableArray[Long] = ImmutableArray(Array.range(from.toInt, to.toInt).map(_.toLong))

    def fromCsv[T: ClassTag](fileName: String,
                             separator: String = ",", skipHeaderLines: Int = 1)
                            (valueFromRow: ImmutableArray[String] => T): ImmutableArray[T] = {
      val bufferedSource = Source.fromFile(fileName)

      val rows = ImmutableArray(bufferedSource.getLines.map(line => {
        ImmutableArray(line.split(separator, -1))
      }).drop(skipHeaderLines))

      rows.map(valueFromRow)
    }
  }

}
