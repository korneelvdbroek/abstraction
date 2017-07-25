package com.fbot.common.fastcollections

import com.fbot.common.fastcollections.index.ArrayIndex

import scala.reflect.ClassTag

/**
  *
  */
class FastTuple2Zipped[El1, Self1[El1] <: FastTuple[El1, Self1[El1]], El2, Self2[El2] <: FastTuple[El2, Self2[El2]]]
                      (val zipTuple: (Self1[El1], Self2[El2])) extends AnyVal {

  def map[B: ClassTag, Self](f: (El1, El2) => B)(implicit builder: Array[B] => Self): Self = {
    val len = zipTuple._1.length
    val zipped = new Array[B](len)

    var i = 0
    while (i < len) {
      zipped(i) = f(zipTuple._1(ArrayIndex(i)), zipTuple._2(ArrayIndex(i)))
      i += 1
    }

    builder(zipped)
  }


  def foldLeft[Acc: ClassTag](z: Acc)(op: (Acc, El1, El2) => Acc): Acc = {
    val len = zipTuple._1.length
    var acc = z

    var i: Int = 0
    while (i < len) {
      acc = op(acc, zipTuple._1(ArrayIndex(i)), zipTuple._2(ArrayIndex(i)))
      i += 1
    }
    acc
  }

}

object FastTuple2Zipped {

//  class Ops[T1, T2](val zipTuple: (T1, T2)) extends AnyVal {
//    def zippedX[El1, Self1 <: FastTuple[El1, Self1], El2, Self2 <: FastTuple[El2, Self2]]
//      (implicit w1: T1 => FastTuple[El1, Self1], w2: T2 => FastTuple[El2, Self2]): FastTuple2Zipped[El1, Self1, El2, Self2] = {
//      new FastTuple2Zipped((w1(zipTuple._1), w2(zipTuple._2)))
//    }
//  }
//
//  implicit def tuple2ToFastTuple2ZippedOps[T1, T2](x: (T1, T2)): Ops[T1, T2] = new FastTuple2Zipped.Ops(x)

  implicit def tuple2FastTuple2Zipped[El1, Self1[El1] <: FastTuple[El1, Self1[El1]], El2, Self2[El2] <: FastTuple[El2, Self2[El2]]]
                                     (zipTuple: (Self1[El1], Self2[El2])): FastTuple2Zipped[El1, Self1, El2, Self2] = {
    new FastTuple2Zipped(zipTuple._1, zipTuple._2)
  }

}
