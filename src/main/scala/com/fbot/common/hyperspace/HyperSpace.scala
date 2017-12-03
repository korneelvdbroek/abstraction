package com.fbot.common.hyperspace

import com.fbot.common.fastcollections.ImmutableArray
import com.fbot.common.fastcollections.index.ArrayIndex
import ImmutableArray._

/**
  *  HyperSpace is defined by
  *  1. a distance definition
  *  2. an (axes-)embedding in the parent Tuple-space (R**d)
  *  3. a grid of HyperSpaceUnits
  */
trait HyperSpace {

  val origin: Tuple

  val unitCubeSizes: Tuple

  val embeddingAxes: ImmutableArray[ArrayIndex]

  lazy val dim: Int = unitCubeSizes.length

  def hyperSpaceUnitAround(point: Tuple): HyperSpaceUnit = {
    val location = (embed(point) - origin) / unitCubeSizes
    HyperSpaceUnit(location.repr.map(_.floor.toLong))
  }

  def embed(point: Tuple): Tuple = {
    embeddingAxes.map(point(_))
  }

  def embed(spaceUnit: => HyperSpaceUnit): HyperSpaceUnit = {
    embeddingAxes.map(spaceUnit(_))
  }

  def toCoordinate(spaceUnit: HyperSpaceUnit): Tuple = {
    unitCubeSizes * spaceUnit + origin
  }


  def distance(point1: Tuple, point2: Tuple): Double = {
    embeddingAxes.foldLeft(0d)((distance, embeddingAxis) => {
      val d = math.abs(point1(embeddingAxis) - point2(embeddingAxis))
      if (d > distance) d else distance
    })
  }

}

case class Space(embeddingAxes: ImmutableArray[ArrayIndex], origin: Tuple, unitCubeSizes: Tuple) extends HyperSpace

