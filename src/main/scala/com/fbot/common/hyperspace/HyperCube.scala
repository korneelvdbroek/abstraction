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

  def growCubeSidesToIncludeDistanceAround(epsilon: Double, point: Tuple,
                                           unitCubeSize: Array[Double], normalCoordinate: (Tuple, HyperCube) => Int => (Double, Double)): HyperCube = {
    val unitsToGrow = ImmutableArray.range(0, dim).map(axis => {
      val (l, r) = normalCoordinate(point, this)(axis)
      (if (l < epsilon) -((epsilon - l) / unitCubeSize(axis)).floor.toLong - 1L else 0L,
       if (r <= epsilon) ((epsilon - r) / unitCubeSize(axis)).floor.toLong + 1L else 0L)
    })
    grow(unitsToGrow.map(_._1), unitsToGrow.map(_._2))
  }


  def minus(innerCube: HyperCube): ImmutableArray[HyperSpaceUnit] = {
    def cartesianProduct(xs: ImmutableArray[IndexedSeq[Long]]): Seq[HyperSpaceUnit] = {
      xs.foldLeft(Seq(Seq.empty[Long]))((x, y) => {
        for (a <- x.view; b <- y)
          yield a :+ b
      }).map(unitHyperCubeLocation => HyperSpaceUnit(unitHyperCubeLocation.toArray))
    }

    ImmutableArray.indexRange(ArrayIndex(0), ArrayIndex(dim)).flatMap(axisToGrow => {

      if (left(axisToGrow) != innerCube.left(axisToGrow) || innerCube.right(axisToGrow) != right(axisToGrow)) {
        val rangesTuple: ImmutableArray[IndexedSeq[Long]] = ImmutableArray.indexRange(ArrayIndex(0), ArrayIndex(dim)).map(axis => {
          if (axis < axisToGrow) {
            left(axis) until right(axis)
          } else if (axisToGrow == axis) {
            (left(axisToGrow) until innerCube.left(axisToGrow)) ++ (innerCube.right(axisToGrow) until right(axisToGrow))
          } else {
            innerCube.left(axis) until innerCube.right(axis)
          }
        })

        cartesianProduct(rangesTuple)
      } else {

        Seq.empty[HyperSpaceUnit]
      }
    })

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

}
