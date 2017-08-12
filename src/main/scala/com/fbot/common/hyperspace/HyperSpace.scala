package com.fbot.common.hyperspace

import com.fbot.common.fastcollections.ImmutableArray
import com.fbot.common.fastcollections.index.ArrayIndex

/**
  *  HyperSpace is defined by
  *  1. a distance definition
  *  2. an (axes-)embedding in the parent Tuple-space (R^d)
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

}

case class Space(embeddingAxes: ImmutableArray[ArrayIndex], unitCubeSizes: ImmutableArray[Double]) extends HyperSpace {

  def subSpace(subSpaceEmbeddingAxes: ImmutableArray[ArrayIndex], subSpaceUnitCubeSizes: ImmutableArray[Double]): HyperSpace = {
    require(subSpaceEmbeddingAxes.length == subSpaceUnitCubeSizes.length, "Unable to create SubSpace: number of axis and unit sizes do not match")

    Space(subSpaceEmbeddingAxes.map(embeddingAxes(_)), subSpaceUnitCubeSizes)
  }

}

