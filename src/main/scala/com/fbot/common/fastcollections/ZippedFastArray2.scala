package com.fbot.common.fastcollections

import com.fbot.common.fastcollections.index.ArrayIndex

import scala.reflect.ClassTag

/**
  * Copyright (C) 6/6/2017 - REstore NV
  *
  */
class ZippedFastArray2[El1, El2](array1: ImmutableArray[El1], array2: ImmutableArray[El2]) {

  def map[B: ClassTag](f: (El1, El2) => B): ImmutableArray[B] = {

    val len = array1.length
    val zipped = new Array[B](len)

    var i = 0
    while (i < len) {
      zipped(i) = f(array1(ArrayIndex(i)), array2(ArrayIndex(i)))
      i += 1
    }

    new ImmutableArray(zipped)
  }

}

object ZippedFastArray2 {

  implicit def tuple2ZippedImmutableArray[El1, El2](arrayTuple: (ImmutableArray[El1], ImmutableArray[El2])): ZippedFastArray2[El1, El2] = {
    new ZippedFastArray2(arrayTuple._1, arrayTuple._2)
  }

}
