package com.fbot.common.hyperspace

import com.fbot.common.fastcollections.ImmutableArray

/**
  * Copyright (C) 6/10/2017 - REstore NV
  *
  */
case class HyperCube(left: UnitHyperCube, right: UnitHyperCube) {

  def grow(leftDirection: ImmutableArray[Long], rightDirection: ImmutableArray[Long]): HyperCube = {
    HyperCube(left + UnitHyperCube(leftDirection.repr), right + UnitHyperCube(rightDirection.repr))
  }

  def grow(leftDirection: Array[Long], rightDirection: Array[Long]): HyperCube = {
    HyperCube(left + UnitHyperCube(leftDirection), right + UnitHyperCube(rightDirection))
  }

}
