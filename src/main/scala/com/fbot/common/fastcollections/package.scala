package com.fbot.common

import com.fbot.common.fastcollections.ops.immutablearrayops.{ImmutableArrayIndexArrayOps, _}
import com.fbot.common.fastcollections.ops.hyperspaceunitops.HyperSpaceUnitOps
import com.fbot.common.fastcollections.ops.tupleops.TupleOps
import com.fbot.common.fastcollections.core.newtype.Newtype
import com.fbot.common.fastcollections.core.translucenttag.@@
import com.fbot.common.fastcollections.core.newtype._
import com.fbot.common.fastcollections.core.{LiteWrappedArray, newtype, translucenttag}

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
  *
  *
  * Value classes in scala don't really do their job, so we need tagged types
  * https://failex.blogspot.be/2017/04/the-high-cost-of-anyval-subclasses.html
  * We use shapeless since it makes T = Int translucent (scalaz7+ does not),
  * hence the type erasure of the tagged type is the primitive type.
  *
  * specialized does not really do its job in scala, it does not specialize an Array[T] contained in a trait with type T specialized
  * http://www.scala-lang.org/old/node/10408.html
  *
  *
  *
  * Solution needs to support
  * 1. Performance of an Array of primitives
  * 2. Having equals defined based on sameelements
  * 3. Being typesafe by having a separate type at compile time (no performance penalty) and only expose the methods I decide (e.g. to make it immutable)
  *
  * Proposed solution:
  * Newtype around a WrapperArray-Lite which contains an Array.
  * Newtype controls nicely what operations of the underlying WrapperArrayLite that we want to expose, moreover it becomes a separate type with type safety
  * without any boxing overhead. The WrapperArray-Lite is to properly implement equals() and hashcode() on Array -- so this is a real object with boxing.
  *
  */
package object fastcollections {


  /**
    * ArrayIndex
    *
    * @implNote: Since we have plenty of these flying around, we need its erasure to be Int!
    *            Hence we pick the translucent version of tagging (see https://failex.blogspot.be/2017/04/and-glorious-subst-to-come.html)
    */
  trait ArrayIndexTag

  type ArrayIndex = Int @@ ArrayIndexTag

  // add ctor
  @inline final def ArrayIndex(i: Int): ArrayIndex = translucenttag[ArrayIndexTag][Int](i)

  // add methods
  case class ArrayIndexOps(i: Int) extends AnyVal {

    def toInt: Int = i

    def += (j: Int): ArrayIndex = ArrayIndex(i + j)

    def < (j: Int): Boolean = i < j

    def == (j: Int): Boolean = i == j

    def > (j: Int): Boolean = i > j
  }

  implicit def arrayIndexOps(index: ArrayIndex): ArrayIndexOps = ArrayIndexOps(index.asInstanceOf[Int])

  object ArrayIndex {
    def subst[F[_]](fi: F[Int]): F[ArrayIndex] = fi.asInstanceOf[F[ArrayIndex]]

  }

  /**
    * Tuple
    *
    * @implNote: newtype provides a new type-safe type with specific operations without boxing overhead
    *            build on top of LiteWrappedArray[Double]
    */
  type Tuple = Newtype[LiteWrappedArray[Double], TupleOps]

  // add ctors
  private[fastcollections] def Tuple(arr: LiteWrappedArray[Double]): Tuple = newtype[LiteWrappedArray[Double], TupleOps](arr)

  def Tuple(arr: Array[Double]): Tuple = Tuple(new LiteWrappedArray(arr))

  def Tuple(d: Double*): Tuple = Tuple(d.toArray)

  // add methods
  implicit def tupleOps(t: Tuple): TupleOps = TupleOps(t.asInstanceOf[LiteWrappedArray[Double]])

  // add factory
  object Tuple {

    def fill(dim: Int)(elem: ⇒ Double): Tuple = Tuple(Array.fill(dim)(elem))
  }


  /**
    * HyperSpaceUnit
    *
    * @implNote: newtype provides a new type-safe type with specific operations without boxing overhead
    *            build on top of LiteWrappedArray[Long]
    */
  type HyperSpaceUnit = Newtype[LiteWrappedArray[Long], HyperSpaceUnitOps]

  // add ctors
  private[fastcollections] def HyperSpaceUnit(arr: LiteWrappedArray[Long]): HyperSpaceUnit = newtype[LiteWrappedArray[Long], HyperSpaceUnitOps](arr)

  def HyperSpaceUnit(arr: Array[Long]): HyperSpaceUnit = HyperSpaceUnit(new LiteWrappedArray(arr))

  def HyperSpaceUnit(d: Long*): HyperSpaceUnit = HyperSpaceUnit(d.toArray)

  // add methods
  implicit def hyperSpaceUnitOps(t: HyperSpaceUnit): HyperSpaceUnitOps = HyperSpaceUnitOps(t.asInstanceOf[LiteWrappedArray[Long]])

  // add factories
  object HyperSpaceUnit {

    def fill(dim: Int)(elem: ⇒ Long): HyperSpaceUnit = HyperSpaceUnit(Array.fill(dim)(elem))

    def apply(position: Long*): HyperSpaceUnit = HyperSpaceUnit(position.toArray)

    def unit(dim: Int): HyperSpaceUnit = HyperSpaceUnit(Array.fill[Long](dim)(1L))
  }

  /**
    * ImmutableArray
    *
    * @implNote: newtype provides a new type-safe type with specific operations without boxing overhead
    *            build on top of LiteWrappedArray[T] with specializations for T = (Double, Int, Long)
    *
    *            Note: a tuple is a boxed Array[Double] (boxed by LiteWrappedArray), so an ImmutableArray[Tuple]
    *            has each element boxed, hence requiring the more performing ImmutableTupleArray
    */
  type ImmutableArray[T] = Newtype[LiteWrappedArray[T], ImmutableArrayOps]

  // add ctors
  private[fastcollections] def ImmutableArray[@specialized(Double, Int, Long) T](arr: LiteWrappedArray[T]): ImmutableArray[T] = newtype(arr)

  def ImmutableArray[@specialized(Double, Int, Long) T](arr: Array[T]): ImmutableArray[T] = ImmutableArray(new LiteWrappedArray(arr))

  // add methods
  implicit def immutableArrayOps4Generic[T](t: ImmutableArray[T]): ImmutableGenericArrayOps[T] = {
    ImmutableGenericArrayOps(t.asInstanceOf[LiteWrappedArray[T]])
  }

  implicit def immutableArrayOps4Double(t: ImmutableArray[Double]): ImmutableDoubleArrayOps = {
    ImmutableDoubleArrayOps(t.asInstanceOf[LiteWrappedArray[Double]])
  }

  implicit def immutableArrayOps4Int(t: ImmutableArray[Int]): ImmutableIntArrayOps = {
    ImmutableIntArrayOps(t.asInstanceOf[LiteWrappedArray[Int]])
  }

  implicit def immutableArrayOps4ArrayIndex(t: ImmutableArray[ArrayIndex]): ImmutableArrayIndexArrayOps = {
    ImmutableArrayIndexArrayOps(t.asInstanceOf[LiteWrappedArray[Int]])
  }

  implicit def immutableArrayOps4Long(t: ImmutableArray[Long]): ImmutableLongArrayOps = {
    ImmutableLongArrayOps(t.asInstanceOf[LiteWrappedArray[Long]])
  }

  // add factories
  object ImmutableArray {

    def apply[@specialized(Double, Int, Long) A: ClassTag](data0: A, dataRest: A*): ImmutableArray[A] = ImmutableArray[A]((data0 +: dataRest).toArray)

    def apply[A: ClassTag](data: TraversableOnce[A]): ImmutableArray[A] = ImmutableArray[A](data.toArray)

    def fill[@specialized(Double, Int, Long) A: ClassTag](n: Int)(elem: ⇒ A): ImmutableArray[A] = ImmutableArray(Array.fill[A](n)(elem))

    def empty[A: ClassTag]: ImmutableArray[A] = ImmutableArray(new Array[A](0))

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
