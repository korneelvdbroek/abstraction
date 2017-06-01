package com.fbot.algos.mutualinformation

import scala.math.Ordering

/**
  * Copyright (C) 5/31/2017 - REstore NV
  *
  */
final case class TupleIndex(i: Int) extends AnyVal {

  override def toString: String = {
    s"index_$i"
  }

}

object TupleIndex {

  implicit val index: Index[TupleIndex] = new Index[TupleIndex] {
    def plus(i: TupleIndex, j: TupleIndex): TupleIndex = TupleIndex(i.i + j.i)
    def minus(i: TupleIndex, j: TupleIndex): TupleIndex = TupleIndex(i.i - j.i)
    def times(i: TupleIndex, j: TupleIndex): TupleIndex = TupleIndex(i.i * j.i)

    def fromInt(i: Int): TupleIndex = TupleIndex(i)

    def compare(i: TupleIndex, j: TupleIndex): Int = i.i.compare(j.i)
  }

  implicit def mkIndexOps(lhs: TupleIndex)(implicit ind: Index[TupleIndex]): Index[TupleIndex]#Ops = new ind.Ops(lhs)

  implicit def mkOrderingOps(lhs: TupleIndex)(implicit ord: Ordering[TupleIndex]): Ordering[TupleIndex]#Ops = new ord.Ops(lhs)

}
