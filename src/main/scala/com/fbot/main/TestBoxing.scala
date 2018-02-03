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

  // TODO: checks
  val z = ArrayIndex(27)   // should be I

  val a = ImmutableArray[ArrayIndex](Array(ArrayIndex(4), ArrayIndex(11)).map(ArrayIndex))  // should be [I


  //
  val tuple = Tuple(13d, 17d, 23d)   // should be [D

  val tuple2 = tuple.head

  //
  val immutableArray = ImmutableArray[Double](Array(4d, 11d, 8d, 2d, 13d)) // should be [D

  val immutableArray2 = immutableArray.map((x: Double) => x.toInt)

  val immutableArray3 = immutableArray.indexOfSorted
}
