package com.fbot.algos.mutualinformation

/**
  * Copyright (C) 6/3/2017 - REstore NV
  *
  */
trait HyperSpace {
  val dim: Int
  val unitCubeSize: Array[Double]

  def findEnclosingUnitHyperCube(point: Tuple): UnitHyperCube = {
    val axes = Array.range(0, dim)
    val position = axes.map(axis => (point(axis) / unitCubeSize(axis)).floor)
    UnitHyperCube(position)
  }
}

case class Space(dim: Int, unitCubeSize: Array[Double]) extends HyperSpace

