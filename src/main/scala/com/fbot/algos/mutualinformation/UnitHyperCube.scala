package com.fbot.algos.mutualinformation

import com.fbot.common.immutable.ImmutableArray
import com.fbot.common.immutable.LongArrayMath._

/**
  * Copyright (C) 6/2/2017 - REstore NV
  *
  */
case class UnitHyperCube(position: Array[Long]) extends AnyVal {

  def move(direction: Array[Long]): UnitHyperCube = {
    UnitHyperCube(position + direction)
  }

}





case class HyperCube(left: UnitHyperCube, right: UnitHyperCube) {

  def grow(leftDirection: Array[Long], rightDirection: Array[Long]): HyperCube ={
    val newCube = HyperCube(left.move(leftDirection), right.move(rightDirection))



    newCube
  }

}

trait HyperCubeOps {

  /**
    * cube1: p1  p2  p3
    *         3   0   0
    *         4   1
    *         5   2
    *             3
    *             4
    *
    *
    *
    * @param cube1
    * @param cube2
    * @return
    */
  def minus(smallCube: HyperCube, bigCube: HyperCube): ImmutableArray[UnitHyperCube] = {
    (bigCube.right.position - bigCube.left.position)(axis)
    ???
  }

}
