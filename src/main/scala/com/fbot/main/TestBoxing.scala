package com.fbot.main

import com.fbot.common.fastcollections.{ArrayIndex, ImmutableArray}
import com.fbot.common.fastcollections._

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
object TestBoxing extends App {

  // ArrayIndex
  var a0 = ArrayIndex(4)           // should be I
  var a1 = ArrayIndex(11)
  val arr = Array(a0, a1)          // should be [I                    (since translucent. It still kinda behaves like an Int too)
  val imArr = ImmutableArray(arr)  // should be [Ljava.lang.Object    (not translucent, since we don't want it to behave as an Array[Double] but strictly control its methods)
  val imArrGut = imArr.repr        // should be [I                    (the insides of an ImmutableArray)
  val len = imArr.length

  // Tuple
  val tuple = Tuple(13d, 17d, 23d)   // should be [Ljava.lang.Object  (cannot be translucent since it would kinda behave like an Array[Double]... mutable etc.)

  val tuple2 = tuple.head

  // ImmutableArray
  val immutableArray = ImmutableArray[Double](Array(4d, 11d, 8d, 2d, 13d)) // should be [Ljava.lang.Object

  val immutableArray2 = immutableArray.map((x: Double) => x.toInt)

  val immutableArray3 = immutableArray.indexOfSorted    // no ImmutableArrayOps is instantiated (since it's a value class :-)
}
