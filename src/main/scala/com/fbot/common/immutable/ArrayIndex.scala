package com.fbot.common.immutable

import scala.math.Ordering

/**
  * Copyright (C) 5/31/2017 - REstore NV
  *
  */
final case class ArrayIndex(i: Int) extends AnyVal {

  override def toString: String = {
    s"index_$i"
  }

}

object ArrayIndex {

  implicit val index: Index[ArrayIndex] = new Index[ArrayIndex] {
    def plus(i: ArrayIndex, j: ArrayIndex): ArrayIndex = ArrayIndex(i.i + j.i)
    def minus(i: ArrayIndex, j: ArrayIndex): ArrayIndex = ArrayIndex(i.i - j.i)
    def times(i: ArrayIndex, j: ArrayIndex): ArrayIndex = ArrayIndex(i.i * j.i)

    def fromInt(i: Int): ArrayIndex = ArrayIndex(i)

    def compare(i: ArrayIndex, j: ArrayIndex): Int = i.i.compare(j.i)
  }

  implicit def mkIndexOps(lhs: ArrayIndex)(implicit ind: Index[ArrayIndex]): Index[ArrayIndex]#Ops = new ind.Ops(lhs)

  implicit def mkOrderingOps(lhs: ArrayIndex)(implicit ord: Ordering[ArrayIndex]): Ordering[ArrayIndex]#Ops = new ord.Ops(lhs)

}