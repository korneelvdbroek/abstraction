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
class TupleSpec extends FlatSpec with Matchers {

  import TupleSpec._

  "===" should "return true for the same object Tuple" in {
    tuple1 === tuple1 shouldBe true
  }

  it should "return true for identical Tuples" in {
    tuple1 === tuple2 shouldBe true
  }

  it should "return false for Tuples with same elements but in different order" in {
    tuple1 === tuple3 shouldBe false
  }

}

object TupleSpec {
  val tuple1 = Tuple(13d, 17d, 23d)
  val tuple2 = Tuple(13d, 17d, 23d)
  val tuple3 = Tuple(17d, 13d, 23d)

}
