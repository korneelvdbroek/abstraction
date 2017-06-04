package com.fbot.algos.mutualinformation

import com.fbot.common.immutable.DoubleArrayMath._

/**
  * Copyright (C) 6/2/2017 - REstore NV
  *
  */
case class UnitHyperCube(position: Array[Double]) extends AnyVal {

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

  def apply(position: Double*): UnitHyperCube = new UnitHyperCube(position.toArray)

}


case class HyperCube(left: UnitHyperCube, right: UnitHyperCube) {

  def grow(leftDirection: Array[Double], rightDirection: Array[Double]): HyperCube = {
    HyperCube(UnitHyperCube(left.position + leftDirection), UnitHyperCube(right.position + rightDirection))
  }

}
