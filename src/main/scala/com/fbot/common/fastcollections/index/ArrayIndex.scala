package com.fbot.common.fastcollections.index

import scala.math.Ordering

/**
  * Copyright (C) 5/31/2017 - REstore NV
  *
  * ArrayIndex is a value class to make ImmutableArray indexing type safe
  */
final case class ArrayIndex(i: Int) extends AnyVal {

  override def toString: String = {
    s"index_$i"
  }

}

object ArrayIndex {

  implicit val index: Indexing[ArrayIndex] = new Indexing[ArrayIndex] {
    def plus(i: ArrayIndex, j: ArrayIndex): ArrayIndex = ArrayIndex(i.i + j.i)
    def minus(i: ArrayIndex, j: ArrayIndex): ArrayIndex = ArrayIndex(i.i - j.i)
    def times(i: ArrayIndex, j: ArrayIndex): ArrayIndex = ArrayIndex(i.i * j.i)

    def fromInt(i: Int): ArrayIndex = ArrayIndex(i)

    def compare(i: ArrayIndex, j: ArrayIndex): Int = i.i.compare(j.i)
  }

  implicit def mkIndexOps(lhs: ArrayIndex)(implicit ind: Indexing[ArrayIndex]): Indexing[ArrayIndex]#Ops = new ind.Ops(lhs)

  implicit def mkOrderingOps(lhs: ArrayIndex)(implicit ord: Ordering[ArrayIndex]): Ordering[ArrayIndex]#Ops = new ord.Ops(lhs)

}