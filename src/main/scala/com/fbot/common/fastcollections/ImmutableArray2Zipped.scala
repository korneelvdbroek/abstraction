package com.fbot.common.fastcollections

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
class ImmutableArray2Zipped[El1, El2](val zipArray: (ImmutableArray[El1], ImmutableArray[El2])) extends AnyVal {

  def length: Int = zipArray._1.length

  def apply(index: ArrayIndex): (El1, El2) = {
    (zipArray._1(index), zipArray._2(index))
  }

  def foreach(f: (El1, El2) ⇒ Unit): Unit = {
    val len = length

    var i: Int = 0
    while (i < len) {
      f(zipArray._1(ArrayIndex(i)), zipArray._2(ArrayIndex(i)))
      i += 1
    }
  }

  def foreachWithIndex(f: (El1, El2, Int) ⇒ Unit): Unit = {
    val len = length

    var i: Int = 0
    while (i < len) {
      f(zipArray._1(ArrayIndex(i)), zipArray._2(ArrayIndex(i)), i)
      i += 1
    }
  }

  def map[B: ClassTag](f: (El1, El2) => B): ImmutableArray[B] = {
    val zipped = new Array[B](length)

    foreachWithIndex((elem1, elem2, index) => {
      zipped(index) = f(elem1, elem2)
    })

    ImmutableArray(zipped)
  }


  def foldLeft[Acc: ClassTag](z: Acc)(op: (Acc, El1, El2) => Acc): Acc = {
    var acc = z
    foreach((elem1, elem2) => {
      acc = op(acc, elem1, elem2)
    })
    acc
  }

  def groupBy[Key: ClassTag](f: (El1, El2) => Key)
                            (implicit evidence1: ClassTag[El1], evidence2: ClassTag[El2]): Map[Key, (ImmutableArray[El1], ImmutableArray[El2])] = {
    ImmutableArray.range(0, zipArray._1.length)
      .groupBy(index => f(zipArray._1(ArrayIndex(index)), zipArray._2(ArrayIndex(index))))
      .mapValues(groupedIndices => (groupedIndices.map(index => zipArray._1(ArrayIndex(index))), groupedIndices.map(index => zipArray._2(ArrayIndex(index)))))
  }

  def filter(f: (El1, El2) ⇒ Boolean)(implicit evidence1: ClassTag[El1], evidence2: ClassTag[El2]): ImmutableArray2Zipped[El1, El2] = {
    val arrayBuilder1 = Array.newBuilder[El1]
    val arrayBuilder2 = Array.newBuilder[El2]
    foreach((elem1, elem2) => {
      if (f(elem1, elem2)) {
        arrayBuilder1 += elem1
        arrayBuilder2 += elem2
      }
    })

    new ImmutableArray2Zipped(ImmutableArray(arrayBuilder1.result), ImmutableArray(arrayBuilder2.result))
  }

  def sliceWhile(p: (El1, El2) => Boolean, from: ArrayIndex)(implicit evidence1: ClassTag[El1], evidence2: ClassTag[El2]): ImmutableArray2Zipped[El1, El2] = {
    val arrayBuilder1 = Array.newBuilder[El1]
    val arrayBuilder2 = Array.newBuilder[El2]

    var index = from
    while (index < length && p(zipArray._1.apply(index), zipArray._2(index))) {
      arrayBuilder1 += zipArray._1(index)
      arrayBuilder2 += zipArray._2(index)
      //      index += 1
      index = ArrayIndex(index.toInt + 1)
    }

    new ImmutableArray2Zipped(ImmutableArray(arrayBuilder1.result), ImmutableArray(arrayBuilder2.result))
  }

  def indicesWhere(p: (El1, El2) ⇒ Boolean): ImmutableArray[Int] = {
    val builder = new mutable.WrappedArrayBuilder[Int](ClassTag(classOf[Int]))
    foreachWithIndex((elem1, elem2, index) => {
      if (p(elem1, elem2)) builder += index
    })
    ImmutableArray(builder.result())
  }

}


object ImmutableArray2Zipped {

  implicit def tuple2ImmutableArray2Zipped[El1, El2](zipArray: (ImmutableArray[El1], ImmutableArray[El2])): ImmutableArray2Zipped[El1, El2] = {
    new ImmutableArray2Zipped(zipArray._1, zipArray._2)
  }
}
