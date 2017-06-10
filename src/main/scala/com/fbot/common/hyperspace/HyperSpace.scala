package com.fbot.common.hyperspace

import com.fbot.common.fastcollections.ImmutableArray
import com.fbot.common.fastcollections.math.FastArrayLongMath

/**
  * Copyright (C) 6/3/2017 - REstore NV
  *
  */
trait HyperSpace {

  val unitCubeSize: Array[Double]

  lazy val dim: Int = unitCubeSize.length

  lazy val axes = ImmutableArray(Array.range(0, dim))

  def findEnclosingUnitHyperCube(point: Tuple): UnitHyperCube = {
    val position = axes.map(axis => (point(axis) / unitCubeSize(axis)).floor.toLong)
    UnitHyperCube(position.repr)
  }

  def emptyCube(tuple: Tuple): HyperCube = {
    val unitHyperCube = findEnclosingUnitHyperCube(tuple)
    HyperCube(unitHyperCube, unitHyperCube)
  }

  def unitCube(tuple: Tuple): HyperCube = {
    val unitHyperCube = findEnclosingUnitHyperCube(tuple)
    HyperCube(unitHyperCube, unitHyperCube + UnitHyperCube(Array.fill[Long](dim)(1L)))
  }
}

case class Space(unitCubeSize: Array[Double]) extends HyperSpace

