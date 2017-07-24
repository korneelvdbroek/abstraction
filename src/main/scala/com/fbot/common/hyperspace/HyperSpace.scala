package com.fbot.common.hyperspace

import com.fbot.common.fastcollections.ImmutableArray
import com.fbot.common.fastcollections.index.ArrayIndex

/**
  *  HyperSpace is defined by
  *  1. a distance definition
  *  2. an (axes-)embedding in the parent space
  *  3. a grid of HyperSpaceUnits
  */
trait HyperSpace {

  val unitCubeSizes: ImmutableArray[Double]

  val embeddingAxes: ImmutableArray[ArrayIndex]

  lazy val dim: Int = unitCubeSizes.length

  def hyperSpaceUnitAround(point: Tuple): HyperSpaceUnit = {
    val position = unitCubeSizes.mapWithIndex((unitCubeSize, axis) => (point(embeddingAxes(axis)) / unitCubeSize).floor.toLong)
    HyperSpaceUnit(position.repr)
  }


  def distance(point1: Tuple, point2: Tuple): Double = {
    embeddingAxes.foldLeft(0d)((distance, embeddingAxis) => {
      val d = math.abs(point1(embeddingAxis) - point2(embeddingAxis))
      if (d > distance) d else distance
    })
  }

  /**
    * Coordinate of point along the normals on each of the sides of the hyperCube.
    * Coordinate is negative on the side outside the cube, and positive on the other side.
    *
    * @param point      point to compute normal coordinate of
    * @param hyperCube  the hyperplanes for the normals are the sides of this cube
    * @param axisIndex       axis
    * @return           the two normal coordinates corresponding to the normal coordinates of the point wrt the two hyperplanes of the cube along axis
    */
  def normalCoordinate(point: Tuple, hyperCube: HyperCube)(axisIndex: ArrayIndex): (Double, Double) = {
    val coordinate = point(embeddingAxes(axisIndex))
    val unitLength = unitCubeSizes(axisIndex)
    (coordinate - hyperCube.left(embeddingAxes(axisIndex)) * unitLength, hyperCube.right(embeddingAxes(axisIndex)) * unitLength - coordinate)
  }

}

case class Space(unitCubeSizes: ImmutableArray[Double], embeddingAxes: ImmutableArray[ArrayIndex]) extends HyperSpace

