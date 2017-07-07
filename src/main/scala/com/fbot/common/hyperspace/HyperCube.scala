package com.fbot.common.hyperspace

import com.fbot.common.fastcollections.ImmutableArray
import com.fbot.common.fastcollections.index.ArrayIndex

/**
  *
  */
case class HyperCube(left: HyperSpaceUnit, right: HyperSpaceUnit) {

  def dim: Int = left.dim

  def grow(leftDirection: ImmutableArray[Long], rightDirection: ImmutableArray[Long]): HyperCube = {
    HyperCube(left + HyperSpaceUnit(leftDirection.repr), right + HyperSpaceUnit(rightDirection.repr))
  }

  def minus(innerCube: HyperCube): ImmutableArray[HyperSpaceUnit] = {
    def cartesianProduct(xs: Traversable[Traversable[Long]]): Seq[HyperSpaceUnit] = {
      xs.foldLeft(Seq(Seq.empty[Long]))((x, y) => {
        for (a <- x.view; b <- y)
          yield a :+ b
      }).map(unitHyperCubeLocation => HyperSpaceUnit(unitHyperCubeLocation.toArray))
    }

    val leftDirection = innerCube.left - left
    val rightDirection = right - innerCube.right

    ImmutableArray.indexRange(ArrayIndex(0), ArrayIndex(dim)).flatMap(axisToGrow => {

      val positionRange: ImmutableArray[IndexedSeq[Long]] = ImmutableArray.indexRange(ArrayIndex(0), ArrayIndex(dim)).map(axis => {

        if (axis < axisToGrow) {
          left(axis) + leftDirection(axis) until right(axis) + rightDirection(axis)
        } else if (axisToGrow == axis) {
          ((left(axis) + leftDirection(axis)) until left(axis)) ++ (right(axisToGrow) until (right(axis) + rightDirection(axis)))
        } else {
          left(axis) until right(axis)
        }
      })

      cartesianProduct(positionRange)
    })

  }

  def contains(unitHyperSpace: HyperSpaceUnit): Boolean = {
    unitHyperSpace.forallWithIndex((position, axis) => {
      left(axis) <= position && position < right(axis)
    })
  }

  def growCubeSidesToIncludeDistanceAround(epsilon: Double, point: Tuple, distance: (Tuple, HyperCube) => Int => (Double, Double)): (HyperCube, ImmutableArray[HyperSpaceUnit]) = {
    val unitsToGrow = ImmutableArray.range(0, dim).map(axis => {
      val (l, r) = distance(point, this)(axis)
      (-(l / epsilon).floor.toLong, (r / epsilon).floor.toLong)
    })

    if (unitsToGrow.forall(_ == (0L, 0L))) {
      (this, ImmutableArray.empty)
    } else {
      grow(unitsToGrow.map(_._1), unitsToGrow.map(_._2))
    }
  }

}

object HyperCube {

  def from(hyperSpaceUnit: HyperSpaceUnit): HyperCube = {
    HyperCube(hyperSpaceUnit, hyperSpaceUnit + HyperSpaceUnit.unit(hyperSpaceUnit.length))
  }

}
