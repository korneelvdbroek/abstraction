package com.fbot.common.fastcollections

import org.scalatest.{FlatSpec, Matchers}

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
class ImmutableTupleArraySpec extends FlatSpec with Matchers {

  import ImmutableTupleArraySpec._

  "++" should "concatenate two tuple arrays" in {
    println(arr1)
    println(arr1 ++ arr2)
  }

}

object ImmutableTupleArraySpec {

  val arr1 = ImmutableTupleArray.fromTuples(ImmutableArray(Tuple(0, 1), Tuple(1, 2), Tuple(2, 3)))
  val arr2 = ImmutableTupleArray.fromTuples(ImmutableArray(Tuple(0, 1), Tuple(1, 2), Tuple(2, 4)))

}
