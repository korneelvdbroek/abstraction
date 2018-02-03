package com.fbot.common

import com.fbot.common.fastcollections.fastarrayops._
import com.fbot.common.fastcollections.tupleops.{HyperSpaceUnitOps, TupleOps}
//import shapeless.newtype.Newtype
//import shapeless.newtype
//import shapeless.tag
//import shapeless.tag.@@

import com.fbot.common.fastcollections.newtype.Newtype
import com.fbot.common.fastcollections.newtype

import com.fbot.common.fastcollections.translucenttag
import com.fbot.common.fastcollections.translucenttag.@@

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
  */
package object fastcollections {


  /**
    * ArrayIndex
    *
    * Since we have plenty of these flying around, we need its erasure to be Int!
    * Hence we pick the translucent version of tagging (see https://failex.blogspot.be/2017/04/and-glorious-subst-to-come.html)
    */
  trait ArrayIndexTag

  type ArrayIndex = Int @@ ArrayIndexTag

  // add ctor
  @inline
  def ArrayIndex(i: Int): ArrayIndex = translucenttag[ArrayIndexTag][Int](i)

  // add methods
  case class ArrayIndexOps(i: Int) {

    def toInt: Int = i

    def += (j: Int): ArrayIndex = ArrayIndex(i + j)

    def < (j: Int): Boolean = i < j

    def == (j: Int): Boolean = i == j

    def > (j: Int): Boolean = i > j
  }

  implicit def arrayIndexOps(index: ArrayIndex): ArrayIndexOps = ArrayIndexOps(index.asInstanceOf[Int])


  /**
    * Tuple
    *
    */
  type Tuple = Newtype[Array[Double], TupleOps]

  // add ctors
  def Tuple(arr: Array[Double]): Tuple = newtype[Array[Double], TupleOps](arr)

  def Tuple(d: Double*): Tuple = Tuple(d.toArray)

  // add methods
  implicit def tupleOps(t: Tuple): TupleOps = TupleOps(t.asInstanceOf[Array[Double]])

  // add factory
  object Tuple {

    def fill(dim: Int)(elem: ⇒ Double): Tuple = Tuple(Array.fill(dim)(elem))
  }


  /**
    * HyperSpaceUnit
    */
  type HyperSpaceUnit = Newtype[Array[Long], HyperSpaceUnitOps]

  // add ctors
  def HyperSpaceUnit(arr: Array[Long]): HyperSpaceUnit = newtype[Array[Long], HyperSpaceUnitOps](arr)

  def HyperSpaceUnit(d: Long*): HyperSpaceUnit = HyperSpaceUnit(d.toArray)

  // add methods
  implicit def hyperSpaceUnitOps(t: HyperSpaceUnit): HyperSpaceUnitOps = HyperSpaceUnitOps(t.asInstanceOf[Array[Long]])

  // add factories
  object HyperSpaceUnit {

    def fill(dim: Int)(elem: ⇒ Long): HyperSpaceUnit = HyperSpaceUnit(Array.fill(dim)(elem))

    def apply(position: Long*): HyperSpaceUnit = HyperSpaceUnit(position.toArray)

    def unit(dim: Int): HyperSpaceUnit = HyperSpaceUnit(Array.fill[Long](dim)(1L))
  }

  /**
    * ImmutableArray
    *
    * We don't want ImmutableArray to be translucent.
    * Reason:   If we would make it translucent, then, at compile time, we know it is an Array[T], and hence it will find implicit conversion to ArrayOps, so
    *           all methods from ArrayOps would be accessible such as update(). Moreover, Array[T] has the member method update() which we don't want to expose
    * Drawback: Since we don't make it translucent, an ImmutableArray[T] is not a plain Array[T], instead it is wrapped as a java.lang.Object.
    *           This should be fine performance-wise, as ImmutableArrays can be boxed, but not its content.
    *
    * Note: same holds for Tuples, they will be boxed, so an ImmutableArray[Tuple] has each element boxed, hence requiring the more performant ImmutableTupleArray
    */
  type ImmutableArray[T] = Newtype[Array[T], FastArrayOps]

  def ImmutableArray[@specialized(Double, Int, Long) T](arr: Array[T]): ImmutableArray[T] = newtype(arr)

  implicit def immutableArrayOps4Generic[T](t: ImmutableArray[T]): FastGenericArrayOps[T] = FastGenericArrayOps(t.asInstanceOf[Array[T]])

  implicit def immutableArrayOps4Double(t: ImmutableArray[Double]): FastDoubleArrayOps = FastDoubleArrayOps(t.asInstanceOf[Array[Double]])

  implicit def immutableArrayOps4Int(t: ImmutableArray[Int]): FastIntArrayOps = FastIntArrayOps(t.asInstanceOf[Array[Int]])

  implicit def immutableArrayOps4Long(t: ImmutableArray[Long]): FastLongArrayOps = FastLongArrayOps(t.asInstanceOf[Array[Long]])

  // add factories
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

//  trait ImmutableArrayTag
//
//  type ImmutableArray[T] = Array[T] @@ ImmutableArrayTag
//
//  // add ctor
//  def ImmutableArray[@specialized(Double, Int, Long) T](arr: Array[T]): ImmutableArray[T] = tag[ImmutableArrayTag][Array[T]](arr)
//
//  // add methods
//
//  //TODO: problem is that refArrayOps (ArrayOps) has higher priority for conversion :-( Also ImmutableArray[ArrayIndex] has issues...
//  // http://www.lihaoyi.com/post/ImplicitDesignPatternsinScala.html#type-class-implicits
//  // https://stackoverflow.com/questions/1886953/is-there-a-way-to-control-which-implicit-conversion-will-be-the-default-used
//  implicit def immutableArrayOps4Generic[T <: AnyRef](t: ImmutableArray[T]): FastGenericArrayOps[T] = FastGenericArrayOps(t.asInstanceOf[Array[T]])
//
//  implicit def immutableArrayOps4Double(t: ImmutableArray[Double]): FastDoubleArrayOps = FastDoubleArrayOps(t.asInstanceOf[Array[Double]])
//
//  implicit def immutableArrayOps4Int(t: ImmutableArray[Int]): FastIntArrayOps = FastIntArrayOps(t.asInstanceOf[Array[Int]])
//
//  implicit def immutableArrayOps4Long(t: ImmutableArray[Long]): FastLongArrayOps = FastLongArrayOps(t.asInstanceOf[Array[Long]])
//
//
//  // add factories
//  object ImmutableArray {
//    def apply[@specialized(Double, Int, Long) A: ClassTag](data0: A, dataRest: A*): ImmutableArray[A] = ImmutableArray[A]((data0 +: dataRest).toArray)
//
//    def apply[@specialized(Double, Int, Long) A: ClassTag](data: TraversableOnce[A]): ImmutableArray[A] = ImmutableArray[A](data.toArray)
//
//    def fill[@specialized(Double, Int, Long) A: ClassTag](n: Int)(elem: ⇒ A): ImmutableArray[A] = ImmutableArray(Array.fill[A](n)(elem))
//
//    def empty[@specialized(Double, Int, Long) A: ClassTag]: ImmutableArray[A] = ImmutableArray(new Array[A](0))
//
//    def indexRange(length: Int): ImmutableArray[ArrayIndex] = ImmutableArray(Array.range(0, length).map(ArrayIndex))
//
//    def range(from: Int, to: Int): ImmutableArray[Int] = ImmutableArray(Array.range(from, to))
//
//    def range(from: Long, to: Long): ImmutableArray[Long] = ImmutableArray(Array.range(from.toInt, to.toInt).map(_.toLong))
//
//    def fromCsv[T: ClassTag](fileName: String,
//                             separator: String = ",", skipHeaderLines: Int = 1)
//                            (valueFromRow: ImmutableArray[String] => T): ImmutableArray[T] = {
//
//      val bufferedSource = Source.fromFile(fileName)
//
//      val rows = ImmutableArray(bufferedSource.getLines.map(line => {
//        ImmutableArray(line.split(separator, -1))
//      }).drop(skipHeaderLines))
//
//      rows.map(valueFromRow)
//    }
//  }

}
