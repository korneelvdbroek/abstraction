package com.fbot.common

import shapeless.newtype.{Newtype, newtypeOps}
import shapeless.tag.@@
import shapeless.{newtype, tag}

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

  // ArrayIndex
  sealed trait ArrayIndexTag
  type ArrayIndex = Int @@ ArrayIndexTag
  def ArrayIndex(i: Int): ArrayIndex = tag[ArrayIndexTag][Int](i)


  // Tuple
  type Tuple = Newtype[Array[Double], TupleOps]
  def Tuple(arr: Array[Double]): Tuple = newtype(arr)
  implicit val tupleOps = TupleOps

  def Tuple(d: Double*): Tuple = Tuple(d.toArray)

  object Tuple {
    def fill(dim: Int)(elem: ⇒ Double): Tuple = Tuple(Array.fill(dim)(elem))
  }





  // MyString is a new type with String as its underlying representation and with its operations
  // provided by MyStringOps
  type ImmutableArrayX[T] = Newtype[Array[T], ImmutableArrayXOps[T]]

  // MyString constructor
  def ImmutableArrayX[T](arr: Array[T]): ImmutableArrayX[T] = newtype(arr)

  // Expose String#size as MyString#mySize. No other operations of String are accessible
  trait ImmutableArrayXOps[@specialized(Double) T] {

    def arr: Array[T]

    def mapWithIndex(f: (T, Int) ⇒ T)(implicit evidence: scala.reflect.ClassTag[T]): ImmutableArrayX[T] = {
      val len = arr.length
      val mapped = new Array[T](len)

      var i = 0
      while (i < len) {
        mapped(i) = f(arr(i), i)
        i += 1
      }

      ImmutableArrayX(mapped)
    }

    def add(other: ImmutableArrayX[T])(implicit evidence: scala.reflect.ClassTag[T], num: Numeric[T]): ImmutableArrayX[T] = {
      val len = arr.length
      val res: Array[T] = new Array[T](len)

      var i: Int = 0
      while (i < len) {
        res(i) = num.plus(arr(i), other.arr(i))
        i += 1
      }

      ImmutableArrayX(res)
    }
  }

  case class ImmutableArrayXOpsGeneric[T](arr: Array[T]) extends ImmutableArrayXOps[T]

  case class ImmutableArrayXOpsDouble(arr: Array[Double]) extends ImmutableArrayXOps[Double]

  //  case class ImmutableArrayXOpsInt(arr: Array[Int]) extends ImmutableArrayXOps[Int]
  //  case class ImmutableArrayXOpsLong(arr: Array[Long]) extends ImmutableArrayXOps[Long]

  implicit def toImmutableArrayXOps[@specialized(Double) T](arr: Array[T]): ImmutableArrayXOpsGeneric[T] = ImmutableArrayXOpsGeneric(arr)

  implicit def immutableArrayX2Ops4Double(t: ImmutableArrayX[Double]): ImmutableArrayXOpsDouble = ImmutableArrayXOpsDouble(t.asInstanceOf[Array[Double]])

}
