package com.fbot.common.fastcollections.math

/**
  * Copyright (C) 6/10/2017 - REstore NV
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

