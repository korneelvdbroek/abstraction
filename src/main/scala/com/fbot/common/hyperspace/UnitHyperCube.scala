package com.fbot.common.hyperspace

import com.fbot.common.fastcollections.math.FastArrayLongMath
import com.fbot.common.fastcollections.{FastTuple, ImmutableArray}

import scala.collection.mutable


/**
  * Copyright (C) 6/2/2017 - REstore NV
  *
  * note: we allow Doubles since Int/Long might not be enough for the UnitHyperCube grid...
  */
case class UnitHyperCube(repr: mutable.WrappedArray[Long]) extends AnyVal with FastTuple[Long, UnitHyperCube] with FastArrayLongMath[UnitHyperCube] {

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