package com.fbot.common.immutable

import com.fbot.algos.mutualinformation.Utils
import com.fbot.common.immutable.ZippedImmutableArray2._

import scala.reflect.ClassTag

/**
  * Copyright (C) 6/8/2017 - REstore NV
  *
  */
case class UnzippedMap[A, B: ClassTag](keyArray: ImmutableArray[A], valueArray: ImmutableArray[B], filter: ImmutableArray[Boolean]) {

  def filterKeys(p: (A) ⇒ Boolean): UnzippedMap[A, B] = {
    var time = 0L
    val updatedFilter = (keyArray, filter).map((key, flag) => if (flag) {
      val (result, t) = Utils.timeIt { p(key) }
      time += t
      result
    } else false)
    println(s"predicate: ${ Utils.prettyPrintTime(time )}")
    UnzippedMap(keyArray, valueArray, updatedFilter)
  }

  def filterOut(reject: ImmutableArray[Boolean]): UnzippedMap[A, B] = {
    val updatedFilter = (filter, reject).map((flag, reject) => if (flag) !reject else false)
    UnzippedMap(keyArray, valueArray, updatedFilter)
  }

  def filterOut(reject: (ArrayIndex) => Boolean): UnzippedMap[A, B] = {
    val updatedFilter = filter.mapWithIndex((flag, index) => if (flag) !reject(index) else false)
    UnzippedMap(keyArray, valueArray, updatedFilter)
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

object UnzippedMap {

  def apply[A: ClassTag, B: ClassTag](map: Map[A, B]): UnzippedMap[A, B] = {
    val (keys, values) = map.unzip
    UnzippedMap(ImmutableArray(keys.toArray[A]), ImmutableArray(values.toArray[B]), ImmutableArray.fill(keys.size)(true))
  }

}