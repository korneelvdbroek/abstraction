package com.fbot.algos.mutualinformation

import com.fbot.common.immutable.{ArrayIndex, ImmutableArray}

/**
  * Copyright (C) 6/2/2017 - REstore NV
  *
  */
case class HyperCubeBin(position: Array[Long]) extends AnyVal

case class UnitHyperCube(size: Array[Double], points: ImmutableArray[Tuple]) {

  lazy val binnedTuples: ImmutableArray[HyperCubeBin] = points.indexRange.map(findEnclosingBin)

  def enclosingBin(tupleIndex: ArrayIndex): HyperCubeBin = binnedTuples(tupleIndex)

  private def findEnclosingBin(tupleIndex: ArrayIndex): HyperCubeBin = {
    val axes = Array.range(0, points.dim)
    val position = axes.map(axis => (points(tupleIndex)(axis) / size(axis)).floor.toLong)
    HyperCubeBin(position)
  }

}
