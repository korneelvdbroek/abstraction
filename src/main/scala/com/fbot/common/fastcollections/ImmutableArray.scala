package com.fbot.common.fastcollections

import scala.collection.mutable
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
  */
case class ImmutableArray[@specialized(Double, Int, Long) T](repr: mutable.WrappedArray[T]) extends FastArray[T, ImmutableArray[T]] {

}

object ImmutableArray {

  def apply[@specialized(Double, Int, Long) T: ClassTag](data0: T, dataRest: T*): ImmutableArray[T] = ImmutableArray[T]((data0 +: dataRest).toArray)

  def apply[@specialized(Double, Int, Long) T: ClassTag](data: TraversableOnce[T]): ImmutableArray[T] = ImmutableArray[T](data.toArray)

  def apply[@specialized(Double, Int, Long) T: ClassTag](data: Array[T]): ImmutableArray[T] = ImmutableArray[T](mutable.WrappedArray.make[T](data))

  def fill[T: ClassTag](n: Int)(elem: ⇒ T): ImmutableArray[T] = ImmutableArray(Array.fill[T](n)(elem))

  def empty[T: ClassTag]: ImmutableArray[T] = ImmutableArray(Array.empty[T])

  def range(start: Int, end: Int): ImmutableArray[Int] = ImmutableArray(Array.range(start, end))

  implicit def builder[T: ClassTag](array: Array[T]): ImmutableArray[T] = ImmutableArray(array)

  implicit def asArray[T: ClassTag](immutableArray: ImmutableArray[T]): Array[T] = immutableArray.toArray

  implicit def builderFromArray[T](implicit m: ClassTag[T]): BuilderFromArray[T, ImmutableArray[T]] = {
    new BuilderFromArray[T, ImmutableArray[T]] {
      def result(array: Array[T]): ImmutableArray[T] = ImmutableArray(array)
    }
  }


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


