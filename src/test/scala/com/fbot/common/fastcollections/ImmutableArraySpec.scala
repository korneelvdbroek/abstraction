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
class ImmutableArraySpec extends FlatSpec with Matchers {

  import ImmutableArraySpec._

  "equals" should "return true for the same object" in {
    series1 == series1 shouldBe true
  }

  it should "return true for ImmutableArrays with identical elements in the same order" in {
    series1 == series2 shouldBe true
  }

  it should "return true for ImmutableArrays of different types but with identical elements in the same order" in {
    series1 == series3 shouldBe true
  }

  it should "return false for ImmutableArrays with identical elements in a different order" in {
    series1 == series4 shouldBe false
  }

  "indexOfSorted" should "return the positions of the elements once sorted" in {
    series3.indexOfSorted.toList shouldBe List(ArrayIndex(0), ArrayIndex(1), ArrayIndex(2), ArrayIndex(3))
  }

  "++" should "return a concatenated array" in {
    series1 ++ series2 shouldBe ImmutableArray(0,1,2,3,0,1,2,3)
  }
}

object ImmutableArraySpec {

  val series1 = ImmutableArray(0, 1, 2, 3)
  val series2 = ImmutableArray(0, 1, 2, 3)
  val series3 = ImmutableArray(0d, 1d, 2d, 3d)
  val series4 = ImmutableArray(0, 1, 3, 2)
}

