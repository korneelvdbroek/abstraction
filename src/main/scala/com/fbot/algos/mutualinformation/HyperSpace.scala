package com.fbot.algos.mutualinformation

/**
  * Copyright (C) 6/3/2017 - REstore NV
  *
  */
case class HyperSpace(dim: Int, unitCubeSize: Array[Double]) {

  def findEnclosingUnitHyperCube(point: Tuple): UnitHyperCube = {
    val axes = Array.range(0, dim)
    val position = axes.map(axis => (point(axis) / unitCubeSize(axis)).floor.toLong)
    UnitHyperCube(position)
  }

}

