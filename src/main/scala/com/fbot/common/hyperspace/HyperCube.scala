package com.fbot.common.hyperspace

import com.fbot.common.fastcollections.{HyperSpaceUnit, ImmutableArray}
import com.fbot.common.fastcollections._

/**
  * Copyright (C) 2017-2018  korneelvdbroek
  *
  * This program is free software: you can redistribute it and/or modify
  * it under the terms of the GNU General Public License as published by
  * the Free Software Foundation, either version 3 of the License, or
  * (at your option) any later version.
  *
  * This program is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU General Public License for more details.
  *
  * You should have received a copy of the GNU General Public License
  * along with this program.  If not, see <http://www.gnu.org/licenses/>.
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

    val unitsToGrow = ImmutableArray.range(0, dim).map((axis: Int) => {
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
