package com.fbot.algos.mutualinformation

import com.fbot.common.immutable.LongArrayMath._
import com.fbot.common.immutable.{ArrayIndex, ImmutableArray, ImmutableArrayOps}

import scala.collection.mutable


/**
  * Copyright (C) 6/2/2017 - REstore NV
  *
  * note: we allow Doubles since Int/Long might not be enough for the UnitHyperCube grid...
  */
case class UnitHyperCube(repr: mutable.WrappedArray[Long]) extends AnyVal with ImmutableArrayOps[Long, UnitHyperCube] {

  def make(x: mutable.WrappedArray[Long]): UnitHyperCube = UnitHyperCube(x)

  def isIn(hyperCube: HyperCube): Boolean = {
//    val (result, time) = Utils.timeIt {
//      forallWithIndex((position, axisIndex) => {
//        hyperCube.left(axisIndex) <= position && position < hyperCube.right(axisIndex)
//      })
//    }
//    println(s"isIn: ${ Utils.prettyPrintTime(time )}  $result  ")
//    result
    forallWithIndex((position, axisIndex) => {
      hyperCube.left(axisIndex) <= position && position < hyperCube.right(axisIndex)
    })
  }

  def isNotIn(hyperCube: HyperCube): Boolean = !isIn(hyperCube)

  override def toString: String = {
    repr.repr.mkString("Cube(", ",", ")")
  }

}

object UnitHyperCube {

  def apply(position: Long*): UnitHyperCube = UnitHyperCube(position.toArray)

}





case class HyperCube(left: UnitHyperCube, right: UnitHyperCube) {

  def grow(leftDirection: ImmutableArray[Long], rightDirection: ImmutableArray[Long]): HyperCube = {
    HyperCube(UnitHyperCube(left.repr.toArray + leftDirection.repr.toArray), UnitHyperCube(right.repr.toArray + rightDirection.repr.toArray))
  }

  def grow(leftDirection: Array[Long], rightDirection: Array[Long]): HyperCube = {
    HyperCube(UnitHyperCube(left.repr.toArray + leftDirection), UnitHyperCube(right.repr.toArray + rightDirection))
  }

}