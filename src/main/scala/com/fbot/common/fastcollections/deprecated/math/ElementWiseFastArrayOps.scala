package com.fbot.common.fastcollections.deprecated.math

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
trait ElementWiseFastArrayOps[@specialized T] extends Any {

  /**
    *
    * @param f    element wise operation
    * @param a    array with first elements
    * @param b    array with second elements
    * @param res  array with results
    *             note: needs to be created outside this function, since Array[T] creating requires ClassTag which defeats specialization
    * @return     array with results
    */
  protected def elementWise(f: (T, T) => T)(a: Array[T], b: Array[T])(res: Array[T]): Array[T] = {
    var i: Int = 0
    while (i < a.length) {
      res(i) = f(a(i), b(i))
      i += 1
    }
    res
  }

}

