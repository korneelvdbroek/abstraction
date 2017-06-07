package com.fbot.algos.mutualinformation

import com.fbot.common.immutable.ImmutableArray

/**
  * Copyright (C) 6/3/2017 - REstore NV
  *
  */
trait HyperSpace {
  val dim: Int
  val unitCubeSize: Array[Double]

  lazy val axes = ImmutableArray(Array.range(0, dim))

  def findEnclosingUnitHyperCube(point: Tuple): UnitHyperCube = {
    val position = axes.map(axis => (point(axis) / unitCubeSize(axis)).floor * unitCubeSize(axis))
    UnitHyperCube(position.repr.toArray)
  }
}

case class Space(dim: Int, unitCubeSize: Array[Double]) extends HyperSpace

