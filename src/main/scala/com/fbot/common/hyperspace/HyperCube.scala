package com.fbot.common.hyperspace

import com.fbot.common.fastcollections.ImmutableArray
import com.fbot.common.fastcollections.ImmutableArray._

/**
  *
  */
case class HyperCube(left: HyperSpaceUnit, right: HyperSpaceUnit) {

  def dim: Int = left.dim

  def grow(leftDirection: ImmutableArray[Long], rightDirection: ImmutableArray[Long]): HyperCube = {
    HyperCube(left + HyperSpaceUnit(leftDirection.repr), right + HyperSpaceUnit(rightDirection.repr))
  }

  def growCubeSidesToIncludeDistanceAround(space: HyperSpace)(epsilon: Double, point: Tuple): HyperCube = {
    val embeddedPoint = space.embed(point)
    val (vectorToLeft, vectorToRight) = (embeddedPoint - space.toCoordinate(left), space.toCoordinate(right) - embeddedPoint)

    val unitsToGrow = ImmutableArray.indexRange(0, dim).map(axis => {
      val unitLength = space.unitCubeSizes(axis)
      val (l, r) = (vectorToLeft(axis), vectorToRight(axis))

      (if (l < epsilon) -((epsilon - l) / unitLength).floor.toLong - 1L else 0L,
        if (r <= epsilon) ((epsilon - r) / unitLength).floor.toLong + 1L else 0L)
    })
    grow(unitsToGrow.map(_._1), unitsToGrow.map(_._2))
  }

  def contains(unitHyperSpace: HyperSpaceUnit): Boolean = {
    unitHyperSpace.forallWithIndex((position, axis) => {
      left(axis) <= position && position < right(axis)
    })
  }

}

object HyperCube {

  def from(hyperSpaceUnit: HyperSpaceUnit): HyperCube = {
    HyperCube(hyperSpaceUnit, hyperSpaceUnit + HyperSpaceUnit.unit(hyperSpaceUnit.length))
  }

  def empty(hyperSpaceUnit: HyperSpaceUnit): HyperCube = {
    HyperCube(hyperSpaceUnit, hyperSpaceUnit)
  }

}
