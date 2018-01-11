package com.fbot.common.fastcollections

import com.fbot.common.fastcollections.ImmutableArray._

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
class FastArray2Zipped[El1, Self1[El1] <: FastArray[El1, Self1[El1]], El2, Self2[El2] <: FastArray[El2, Self2[El2]]]
(val zipArray: (Self1[El1], Self2[El2])) extends AnyVal {

  def length: Int = zipArray._1.length

  def apply(index: Int): (El1, El2) = {
    (zipArray._1(index), zipArray._2(index))
  }

  def foreach(f: (El1, El2) ⇒ Unit): Unit = {
    val len = length

    var i: Int = 0
    while (i < len) {
      f(zipArray._1(i), zipArray._2(i))
      i += 1
    }
  }

  def foreachWithIndex(f: (El1, El2, Int) ⇒ Unit): Unit = {
    val len = length

    var i: Int = 0
    while (i < len) {
      f(zipArray._1(i), zipArray._2(i), i)
      i += 1
    }
  }

  def map[B: ClassTag, To](f: (El1, El2) => B)(implicit builder: BuilderFromArray[B, To]): To = {
    val zipped = new Array[B](length)

    foreachWithIndex((elem1, elem2, index) => {
      zipped(index) = f(elem1, elem2)
    })

    builder.result(zipped)
  }


  def foldLeft[Acc: ClassTag](z: Acc)(op: (Acc, El1, El2) => Acc): Acc = {
    var acc = z
    foreach((elem1, elem2) => {
      acc = op(acc, elem1, elem2)
    })
    acc
  }

  def groupBy[Key: ClassTag](f: (El1, El2) => Key)
                            (implicit evidence1: ClassTag[El1], evidence2: ClassTag[El2],
                             builder1: Array[El1] => Self1[El1], builder2: Array[El2] => Self2[El2]): Map[Key, (Self1[El1], Self2[El2])] = {
    ImmutableArray.range(0, zipArray._1.length)
      .groupBy(index => f(zipArray._1(index), zipArray._2(index)))
      .mapValues(groupedIndices => (groupedIndices.map(index => zipArray._1(index)).toArray, groupedIndices.map(index => zipArray._2(index)).toArray))
  }

  def filter(f: (El1, El2) ⇒ Boolean)(implicit evidence1: scala.reflect.ClassTag[El1], builder1: BuilderFromArray[El1, Self1[El1]],
                                      evidence2: scala.reflect.ClassTag[El2],
                                      builder2: BuilderFromArray[El2, Self2[El2]]): FastArray2Zipped[El1, Self1, El2, Self2] = {
    val arrayBuilder1 = Array.newBuilder[El1](evidence1)
    val arrayBuilder2 = Array.newBuilder[El2](evidence2)
    foreach((elem1, elem2) => {
      if (f(elem1, elem2)) {
        arrayBuilder1 += elem1
        arrayBuilder2 += elem2
      }
    })

    new FastArray2Zipped(builder1.result(arrayBuilder1.result), builder2.result(arrayBuilder2.result))
  }

  def sliceWhile(p: (El1, El2) => Boolean, from: Int)(implicit evidence1: scala.reflect.ClassTag[El1], builder1: BuilderFromArray[El1, Self1[El1]],
                                                             evidence2: scala.reflect.ClassTag[El2],
                                                             builder2: BuilderFromArray[El2, Self2[El2]]): FastArray2Zipped[El1, Self1, El2, Self2] = {
    val arrayBuilder1 = Array.newBuilder[El1](evidence1)
    val arrayBuilder2 = Array.newBuilder[El2](evidence2)

    var index = from
    while (index < length && p(zipArray._1.apply(index), zipArray._2(index))) {
      arrayBuilder1 += zipArray._1(index)
      arrayBuilder2 += zipArray._2(index)
      index = index + 1
    }

    new FastArray2Zipped(builder1.result(arrayBuilder1.result), builder2.result(arrayBuilder2.result))
  }

  def indicesWhere(p: (El1, El2) ⇒ Boolean): ImmutableArray[Int] = {
    val builder = new mutable.WrappedArrayBuilder[Int](ClassTag(classOf[Int]))
    foreachWithIndex((elem1, elem2, index) => {
      if (p(elem1, elem2)) builder += index
    })
    ImmutableArray(builder.result())
  }

}


object FastArray2Zipped {

  //  class Ops[T1, T2](val zipTuple: (T1, T2)) extends AnyVal {
  //    def zippedX[El1, Self1 <: FastTuple[El1, Self1], El2, Self2 <: FastTuple[El2, Self2]]
  //      (implicit w1: T1 => FastTuple[El1, Self1], w2: T2 => FastTuple[El2, Self2]): FastTuple2Zipped[El1, Self1, El2, Self2] = {
  //      new FastTuple2Zipped((w1(zipTuple._1), w2(zipTuple._2)))
  //    }
  //  }
  //
  //  implicit def tuple2ToFastTuple2ZippedOps[T1, T2](x: (T1, T2)): Ops[T1, T2] = new FastTuple2Zipped.Ops(x)

  implicit def tuple2FastArray2Zipped[El1, Self1[El1] <: FastArray[El1, Self1[El1]], El2, Self2[El2] <: FastArray[El2, Self2[El2]]]
  (zipArray: (Self1[El1], Self2[El2])): FastArray2Zipped[El1, Self1, El2, Self2] = {
    new FastArray2Zipped(zipArray._1, zipArray._2)
  }

}
