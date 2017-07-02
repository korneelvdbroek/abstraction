package com.fbot.common.hyperspace

import com.fbot.common.fastcollections.ImmutableArray
import com.fbot.common.fastcollections.index.ArrayIndex

/**
  *
  */
case class HyperCube(left: HyperSpaceUnit, right: HyperSpaceUnit) {

  def dim: Int = left.dim

  def grow(leftDirection: ImmutableArray[Long], rightDirection: ImmutableArray[Long]): (HyperCube, ImmutableArray[HyperSpaceUnit]) = {
    def cartesianProduct(xs: Traversable[Traversable[Long]]): Seq[HyperSpaceUnit] = {
      xs.foldLeft(Seq(Seq.empty[Long]))((x, y) => {
        for (a <- x.view; b <- y)
          yield {
            a :+ b
          }
      }).map(unitHyperCubeLocation => HyperSpaceUnit(unitHyperCubeLocation.toArray))
    }

    val newHyperSpaceCubes: ImmutableArray[HyperSpaceUnit] = {
      val newUnitHyperCubes = Array.range(0, dim).flatMap(axisPosition => {
        val i = ArrayIndex(axisPosition)

        val positionRange: Array[IndexedSeq[Long]] = Array.range(0, dim).map(axis => {
          val j = ArrayIndex(axis)

          if (j < i) {
            left(j) + leftDirection(j) until right(j) + rightDirection(j)
          } else if (i == j) {
            ((left(j) + leftDirection(j)) until left(j)) ++ (right(i) until (right(j) + rightDirection(j)))
          } else {
            left(j) until right(j)
          }
        })

        cartesianProduct(positionRange)
      })

      ImmutableArray(newUnitHyperCubes)
    }

    val newHyperCube = HyperCube(left + HyperSpaceUnit(leftDirection.repr), right + HyperSpaceUnit(rightDirection.repr))

    (newHyperCube, newHyperSpaceCubes)
  }

  def contains(unitHyperSpace: HyperSpaceUnit): Boolean = {
    unitHyperSpace.forallWithIndex((position, axis) => {
      left(axis) <= position && position < right(axis)
    })
  }

  def grow(leftDirection: Array[Long], rightDirection: Array[Long]): (HyperCube, ImmutableArray[HyperSpaceUnit]) = {
    grow(ImmutableArray(leftDirection), ImmutableArray(rightDirection))
  }

  def growCubeSidesToIncludeDistanceAround(epsilon: Double, point: Tuple, distance: HyperCube => Int => (Double, Double)): (HyperCube, ImmutableArray[HyperSpaceUnit]) = {
    Array.range(0, dim).map(axis => {
      val (l, r) = distance(this)(axis)
      if
    })

    val growLeft = space.axes.map(axis => if  ((point(axis) - cube.left.repr(axis) * space.unitCubeSize(axis)) < epsilon) -1L else 0L)
    val growRight = space.axes.map(axis => if ((cube.right.repr(axis) * space.unitCubeSize(axis) - point(axis)) < epsilon) 1L else 0L)

    if (growLeft.forall(_ == 0L) && growRight.forall(_ == 0L)) {
      kNearestWithDistance
    } else {
      val (newCube, newUnitCubes) = cube.grow(growLeft, growRight)
    }
    ???
  }

}

object HyperCube {

  def from(hyperSpaceUnit: HyperSpaceUnit): HyperCube = {
    HyperCube(hyperSpaceUnit, hyperSpaceUnit + HyperSpaceUnit.unit(hyperSpaceUnit.length))
  }

}
