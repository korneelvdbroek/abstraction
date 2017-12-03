package com.fbot.common.fastcollections

import com.fbot.common.fastcollections.ImmutableArray._
import com.fbot.common.fastcollections.index.ArrayIndex

import scala.collection.mutable
import scala.reflect.ClassTag

/**
  *
  */
class FastArray2Zipped[El1, Self1[El1] <: FastArray[El1, Self1[El1]], El2, Self2[El2] <: FastArray[El2, Self2[El2]]]
(val zipArray: (Self1[El1], Self2[El2])) extends AnyVal {

  def length: Int = zipArray._1.length

  def apply(index: ArrayIndex): (El1, El2) = {
    (zipArray._1(index), zipArray._2(index))
  }

  def foreach[U](f: (El1, El2) ⇒ U): Unit = {
    val len = length

    var i: Int = 0
    while (i < len) {
      f(zipArray._1(ArrayIndex(i)), zipArray._2(ArrayIndex(i)))
      i += 1
    }
  }

  def foreachWithIndex[U](f: (El1, El2, ArrayIndex) ⇒ U): Unit = {
    val len = length

    var i: Int = 0
    while (i < len) {
      f(zipArray._1(ArrayIndex(i)), zipArray._2(ArrayIndex(i)), ArrayIndex(i))
      i += 1
    }
  }

  def map[B: ClassTag, To](f: (El1, El2) => B)(implicit builder: BuilderFromArray[B, To]): To = {
    val zipped = new Array[B](length)

    foreachWithIndex((elem1, elem2, index) => {
      zipped(index.toInt) = f(elem1, elem2)
    })

    builder.result(zipped)
  }


  def foldLeft[Acc: ClassTag](z: Acc)(op: (Acc, El1, El2) => Acc): Acc = {
    var acc = z
    foreach((elem1, elem2) => {
      acc = op(acc, elem1, elem2)
    })
    acc
  }

  def groupBy[Key: ClassTag](f: (El1, El2) => Key)
                            (implicit evidence1: ClassTag[El1], evidence2: ClassTag[El2],
                             builder1: Array[El1] => Self1[El1], builder2: Array[El2] => Self2[El2]): Map[Key, (Self1[El1], Self2[El2])] = {
    ImmutableArray.indexRange(0, zipArray._1.length)
      .groupBy(index => f(zipArray._1(index), zipArray._2(index)))
      .mapValues(groupedIndices => (groupedIndices.map(index => zipArray._1(index)).toArray, groupedIndices.map(index => zipArray._2(index)).toArray))
  }

  def filter(f: (El1, El2) ⇒ Boolean)(implicit evidence1: scala.reflect.ClassTag[El1], builder1: BuilderFromArray[El1, Self1[El1]],
                                      evidence2: scala.reflect.ClassTag[El2],
                                      builder2: BuilderFromArray[El2, Self2[El2]]): FastArray2Zipped[El1, Self1, El2, Self2] = {
    val arrayBuilder1 = Array.newBuilder[El1](evidence1)
    val arrayBuilder2 = Array.newBuilder[El2](evidence2)
    foreach ((elem1, elem2) => {
      if (f(elem1, elem2)) {
        arrayBuilder1 += elem1
        arrayBuilder2 += elem2
      }
    })

    new FastArray2Zipped(builder1.result(arrayBuilder1.result), builder2.result(arrayBuilder2.result))
  }

  def sliceWhile(p: (El1, El2) => Boolean, from: ArrayIndex)(implicit evidence1: scala.reflect.ClassTag[El1], builder1: BuilderFromArray[El1, Self1[El1]],
                                                             evidence2: scala.reflect.ClassTag[El2],
                                                             builder2: BuilderFromArray[El2, Self2[El2]]): FastArray2Zipped[El1, Self1, El2, Self2] = {
    val arrayBuilder1 = Array.newBuilder[El1](evidence1)
    val arrayBuilder2 = Array.newBuilder[El2](evidence2)

    var index = from
    while (index.toInt < length && p(zipArray._1.apply(index), zipArray._2(index))) {
      arrayBuilder1 += zipArray._1(index)
      arrayBuilder2 += zipArray._2(index)
      index = index.next
    }

    new FastArray2Zipped(builder1.result(arrayBuilder1.result), builder2.result(arrayBuilder2.result))
  }

  def indicesWhere(p: (El1, El2) ⇒ Boolean): ImmutableArray[ArrayIndex] = {
    val builder = new mutable.WrappedArrayBuilder[ArrayIndex](ClassTag(classOf[ArrayIndex]))
    foreachWithIndex((elem1, elem2, index) => {
      if (p(elem1, elem2)) builder += index
    })
    ImmutableArray(builder.result())
  }

}


object FastArray2Zipped {

  //  class Ops[T1, T2](val zipTuple: (T1, T2)) extends AnyVal {
  //    def zippedX[El1, Self1 <: FastTuple[El1, Self1], El2, Self2 <: FastTuple[El2, Self2]]
  //      (implicit w1: T1 => FastTuple[El1, Self1], w2: T2 => FastTuple[El2, Self2]): FastTuple2Zipped[El1, Self1, El2, Self2] = {
  //      new FastTuple2Zipped((w1(zipTuple._1), w2(zipTuple._2)))
  //    }
  //  }
  //
  //  implicit def tuple2ToFastTuple2ZippedOps[T1, T2](x: (T1, T2)): Ops[T1, T2] = new FastTuple2Zipped.Ops(x)

  implicit def tuple2FastArray2Zipped[El1, Self1[El1] <: FastArray[El1, Self1[El1]], El2, Self2[El2] <: FastArray[El2, Self2[El2]]]
  (zipArray: (Self1[El1], Self2[El2])): FastArray2Zipped[El1, Self1, El2, Self2] = {
    new FastArray2Zipped(zipArray._1, zipArray._2)
  }

}
