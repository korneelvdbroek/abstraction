package com.fbot.algos.mutualinformation

import com.fbot.common.immutable.DoubleArrayMath._
import com.fbot.algos.mutualinformation.Tuple._
import com.fbot.common.immutable.ImmutableArray

/**
  * Copyright (C) 6/2/2017 - REstore NV
  *
  * note: we allow Doubles since Int/Long might not be enough for the UnitHyperCube grid...
  */
case class UnitHyperCube(position: Array[Double]) {

  override def hashCode(): Int = scala.util.hashing.MurmurHash3.arrayHashing.hash(position)

  override def equals(that: Any): Boolean = that match {
    case that: UnitHyperCube => (that canEqual this) && (this.position sameElements that.position)
    case _                       => false
  }

  def isIn(hyperCube: HyperCube): Boolean = {
    val axes = Array.range(0, position.length)
    axes.forall(axis => {
      hyperCube.left.position(axis) <= position(axis) && position(axis) < hyperCube.right.position(axis)
    })
  }

  def isNotIn(hyperCube: HyperCube): Boolean = !isIn(hyperCube)

  override def toString: String = {
    position.mkString("Cube(", ",", ")")
  }

}

object UnitHyperCube {

  def apply(position: Double*): UnitHyperCube = UnitHyperCube(position.toArray)

}





case class HyperCube(left: UnitHyperCube, right: UnitHyperCube) {

  def grow(leftDirection: ImmutableArray[Double], rightDirection: ImmutableArray[Double]): HyperCube = {
    HyperCube(UnitHyperCube(left.position + leftDirection.repr.toArray), UnitHyperCube(right.position + rightDirection.repr.toArray))
  }

  def grow(leftDirection: Array[Double], rightDirection: Array[Double]): HyperCube = {
    HyperCube(UnitHyperCube(left.position + leftDirection.repr), UnitHyperCube(right.position + rightDirection.repr))
  }

}

object HyperCube {

  //implicit def apply(left: Array[Double], right: Array[Double]): HyperCube = HyperCube(UnitHyperCube(left), UnitHyperCube(right))

  def empty(tuple: Tuple): HyperCube = {
    HyperCube(UnitHyperCube(tuple), UnitHyperCube(tuple))
  }

  def unit(tuple: Tuple): HyperCube = {
    HyperCube(UnitHyperCube(tuple), UnitHyperCube(tuple + Tuple.one(tuple.dim)))
  }


}
