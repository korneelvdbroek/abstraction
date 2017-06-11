package com.fbot.common.hyperspace

import com.fbot.common.fastcollections.ImmutableArray
import com.fbot.common.fastcollections.ZippedFastArray2._
import com.fbot.common.fastcollections.index.ArrayIndex
import com.fbot.common.fastcollections.math.ArrayLongMath._

/**
  * Copyright (C) 6/10/2017 - REstore NV
  *
  */
case class HyperCube(left: UnitHyperCube, right: UnitHyperCube) {

  def grow(leftDirection: ImmutableArray[Long], rightDirection: ImmutableArray[Long]): (HyperCube, ImmutableArray[UnitHyperCube]) = {
    def cartesianProduct(xs: Traversable[Traversable[Long]]): Seq[UnitHyperCube] = {
      xs.foldLeft(Seq(Seq.empty[Long]))((x, y) => {
        for (a <- x.view; b <- y)
          yield a :+ b
      }).map(unitHyperCubeLocation => UnitHyperCube(unitHyperCubeLocation.toArray))
    }

    def newUnitHyperCubes: ImmutableArray[UnitHyperCube] = {
      val newUnitHyperCubes = Array.range(0, left.length).flatMap(axisPosition => {
        val i = ArrayIndex(axisPosition)

        val positionRange = Array.range(0, left.length).map(axis => {
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

    val newHyperCube = HyperCube(left + UnitHyperCube(leftDirection.repr), right + UnitHyperCube(rightDirection.repr))

    (newHyperCube, newUnitHyperCubes)
  }

  def grow(leftDirection: Array[Long], rightDirection: Array[Long]): (HyperCube, ImmutableArray[UnitHyperCube]) = {
    grow(ImmutableArray(leftDirection), ImmutableArray(rightDirection))
  }

}

object HyperCube {

  implicit def fromUnitHyperCube(unitHyperCube: UnitHyperCube): HyperCube = {
    HyperCube(unitHyperCube, unitHyperCube + UnitHyperCube(Array.fill[Long](unitHyperCube.length)(1L)))
  }

}
