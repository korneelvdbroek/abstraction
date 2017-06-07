package com.fbot.algos.mutualinformation

import com.fbot.common.immutable.DoubleArrayMath._
import com.fbot.common.immutable.ImmutableArray

import scala.collection.mutable

/**
  * Copyright (C) 6/2/2017 - REstore NV
  *
  * note: we allow Doubles since Int/Long might not be enough for the UnitHyperCube grid...
  */
case class UnitHyperCube(position: mutable.WrappedArray[Double]) extends AnyVal {

  def isIn(hyperCube: HyperCube): Boolean = {
    val axes = Array.range(0, position.length)
    axes.forall(axis => {
      hyperCube.left.position(axis) <= position(axis) && position(axis) < hyperCube.right.position(axis)
    })
  }

  def isNotIn(hyperCube: HyperCube): Boolean = !isIn(hyperCube)

  override def toString: String = {
    position.mkString("Cube(", ",", ")")
  }

}

object UnitHyperCube {

  def apply(position: Double*): UnitHyperCube = UnitHyperCube(position.toArray)

}





case class HyperCube(left: UnitHyperCube, right: UnitHyperCube) {

  def grow(leftDirection: ImmutableArray[Double], rightDirection: ImmutableArray[Double]): HyperCube = {
    HyperCube(UnitHyperCube(left.position.toArray + leftDirection.repr.toArray), UnitHyperCube(right.position.toArray + rightDirection.repr.toArray))
  }

  def grow(leftDirection: Array[Double], rightDirection: Array[Double]): HyperCube = {
    HyperCube(UnitHyperCube(left.position.toArray + leftDirection), UnitHyperCube(right.position.toArray + rightDirection))
  }

}