package com.fbot.common.hyperspace

import com.fbot.common.fastcollections.ImmutableArray

/**
  *
  */
case class HyperCube(left: HyperSpaceUnit, right: HyperSpaceUnit) {

  def dim: Int = left.dim

  def grow(leftDirection: ImmutableArray[Long], rightDirection: ImmutableArray[Long]): HyperCube = {
    HyperCube(left + HyperSpaceUnit(leftDirection.repr), right + HyperSpaceUnit(rightDirection.repr))
  }

  def growCubeSidesToIncludeDistanceAround(space: HyperSpace)(epsilon: Double, point: Tuple): HyperCube = {
    val unitsToGrow = ImmutableArray.indexRange(0, dim).map(axis => {

      val coordinate = point(space.embeddingAxes(axis))
      val unitLength = space.unitCubeSizes(axis)
      val (l, r) = (coordinate - left(space.embeddingAxes(axis)) * unitLength, right(space.embeddingAxes(axis)) * unitLength - coordinate)

      (if (l < epsilon) -((epsilon - l) / unitLength).floor.toLong - 1L else 0L,
       if (r <= epsilon) ((epsilon - r) / unitLength).floor.toLong + 1L else 0L)
    })
    grow(unitsToGrow.map(_._1), unitsToGrow.map(_._2))
  }

  def hyperSpaceUnits: ImmutableArray[HyperSpaceUnit] = {
    val rangesTuple: ImmutableArray[IndexedSeq[Long]] = ImmutableArray.indexRange(0, dim).map(axis => {
      left(axis) until right(axis)
    })

    ImmutableArray(cartesianProduct(rangesTuple))
  }

  def minus(innerCube: HyperCube): ImmutableArray[HyperSpaceUnit] = {

    // grow cube axis by axis
    ImmutableArray.indexRange(0, dim).flatMap(axisToGrow => {

      if (left(axisToGrow) != innerCube.left(axisToGrow) || innerCube.right(axisToGrow) != right(axisToGrow)) {
        val rangesTuple: ImmutableArray[IndexedSeq[Long]] = ImmutableArray.indexRange(0, dim).map(axis => {
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


  protected def cartesianProduct(xs: ImmutableArray[IndexedSeq[Long]]): Seq[HyperSpaceUnit] = {
    xs.foldLeft(Seq(Seq.empty[Long]))((x, y) => {
      for (a <- x.view; b <- y)
        yield a :+ b
    }).map(unitHyperCubeLocation => HyperSpaceUnit(unitHyperCubeLocation.toArray))
  }

}

object HyperCube {

  def from(hyperSpaceUnit: HyperSpaceUnit): HyperCube = {
    HyperCube(hyperSpaceUnit, hyperSpaceUnit + HyperSpaceUnit.unit(hyperSpaceUnit.length))
  }

}
