package com.fbot.main

import com.fbot.common.fastcollections.{ImmutableArrayX, immutableArrayX2Ops4Double}

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


  val x = Array(31d, 313d).toList()
  val a = ImmutableArrayX(x)
  val b = ImmutableArrayX(x)

  val c = a add b

  val d = a.mapWithIndex((d, i) => d + i.toDouble)
}
