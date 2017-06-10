package com.fbot.common.fastcollections

import com.fbot.common.fastcollections.ZippedFastArray2._
import com.fbot.common.fastcollections.index.ArrayIndex

import scala.reflect.ClassTag

/**
  * Copyright (C) 6/8/2017 - REstore NV
  *
  */
case class FastMap[A, B: ClassTag](keyArray: ImmutableArray[A], valueArray: ImmutableArray[B], filter: ImmutableArray[Boolean]) {

  def filterKeys(p: (A) ⇒ Boolean): FastMap[A, B] = {
    val updatedFilter = (keyArray, filter).map((key, flag) => if (flag) p(key) else false)
    FastMap(keyArray, valueArray, updatedFilter)
  }

  def filterOut(reject: ImmutableArray[Boolean]): FastMap[A, B] = {
    val updatedFilter = (filter, reject).map((flag, reject) => if (flag) !reject else false)
    FastMap(keyArray, valueArray, updatedFilter)
  }

  def filterOut(reject: (ArrayIndex) => Boolean): FastMap[A, B] = {
    val updatedFilter = filter.mapWithIndex((flag, index) => if (flag) !reject(index) else false)
    FastMap(keyArray, valueArray, updatedFilter)
  }


  def values: ImmutableArray[B] = {
    val b = Array.newBuilder[B]
    val len = keyArray.length

    var i = 0
    while (i < len) {
      if (filter(ArrayIndex(i))) b += valueArray(ArrayIndex(i))
      i += 1
    }

    new ImmutableArray(b.result)
  }

}

object FastMap {

  def apply[A: ClassTag, B: ClassTag](map: Map[A, B]): FastMap[A, B] = {
    val (keys, values) = map.unzip
    FastMap(ImmutableArray(keys.toArray[A]), ImmutableArray(values.toArray[B]), ImmutableArray.fill(keys.size)(true))
  }

}