package com.fbot.algos.mutualinformation

import com.fbot.common.immutable.DoubleArrayMath._
import com.fbot.common.immutable.ImmutableArray

/**
  * Copyright (C) 6/3/2017 - REstore NV
  *
  */
trait HyperSpace {

  val unitCubeSize: Array[Double]

  lazy val dim: Int = unitCubeSize.length

  lazy val axes = ImmutableArray(Array.range(0, dim))

  def findEnclosingUnitHyperCube(point: Tuple): UnitHyperCube = {
    val position = axes.map(axis => (point(axis) / unitCubeSize(axis)).floor * unitCubeSize(axis))
    UnitHyperCube(position.repr.toArray)
  }

  def emptyCube(tuple: Tuple): HyperCube = {
    val unitHyperCube = findEnclosingUnitHyperCube(tuple)
    HyperCube(unitHyperCube, unitHyperCube)
  }

  def unitCube(tuple: Tuple): HyperCube = {
    val unitHyperCube = findEnclosingUnitHyperCube(tuple)
    HyperCube(unitHyperCube, UnitHyperCube(unitHyperCube.position.toArray + unitCubeSize))
  }
}

case class Space(unitCubeSize: Array[Double]) extends HyperSpace

