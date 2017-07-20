package com.fbot.common.hyperspace

import com.fbot.common.fastcollections.{FastTuple2Zipped, ImmutableArray}
import FastTuple2Zipped._

/**
  *
  */
trait HyperSpace {

  // HyperSpace is defined by the edge length of a HyperSpaceUnit!!
  val unitCubeSize: Array[Double]

  lazy val dim: Int = unitCubeSize.length

  lazy val axes = ImmutableArray(Array.range(0, dim))

  def hyperSpaceUnitAround(point: Tuple): HyperSpaceUnit = {
    val position = axes.map(axis => (point(axis) / unitCubeSize(axis)).floor.toLong)
    HyperSpaceUnit(position.repr)
  }


  def distance(point1: Tuple, point2: Tuple): Double = {
    (point1, point2).foldLeft(0d)((distance, point1x, point2x) => {
      val d = math.abs(point1x - point2x)
      if (d > distance) d else distance
    })
  }

  def normalCoordinate(point: Tuple, hyperCube: HyperCube)(axis: Int): (Double, Double) = {
    val coordinate = point(axis)
    val unitLength = unitCubeSize(axis)
    (coordinate - hyperCube.left.repr(axis) * unitLength, hyperCube.right.repr(axis) * unitLength - coordinate)
  }

}

case class Space(unitCubeSize: Array[Double]) extends HyperSpace

