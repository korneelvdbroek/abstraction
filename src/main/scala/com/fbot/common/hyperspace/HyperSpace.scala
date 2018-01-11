package com.fbot.common.hyperspace

import com.fbot.common.fastcollections.ImmutableArray
import com.fbot.common.fastcollections.ImmutableArray._

/**
  * Copyright (C) 2017-2018  korneelvdbroek
  *
  * This program is free software: you can redistribute it and/or modify
  * it under the terms of the GNU General Public License as published by
  * the Free Software Foundation, either version 3 of the License, or
  * (at your option) any later version.
  *
  * This program is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU General Public License for more details.
  *
  * You should have received a copy of the GNU General Public License
  * along with this program.  If not, see <http://www.gnu.org/licenses/>.
  *
  *
  * HyperSpace is defined by
  *  1. a distance definition
  *  2. an (axes-)embedding in the parent Tuple-space (R**d)
  *  3. a grid of HyperSpaceUnits
  */
trait HyperSpace {

  val origin: TupleX

  val unitCubeSizes: TupleX

  val embeddingAxes: ImmutableArray[Int]

  lazy val dim: Int = unitCubeSizes.length

  def hyperSpaceUnitAround(point: TupleX): HyperSpaceUnit = {
    val location = (embed(point) - origin) / unitCubeSizes
    HyperSpaceUnit(location.repr.map(_.floor.toLong))
  }

  def embed(point: TupleX): TupleX = {
    embeddingAxes.map(point.apply)
  }

  def toCoordinate(spaceUnit: HyperSpaceUnit): TupleX = {
    unitCubeSizes * spaceUnit + origin
  }


  def distance(point1: TupleX, point2: TupleX): Double = {
    embeddingAxes.foldLeft(0d)((distance, embeddingAxis) => {
      val d = math.abs(point1(embeddingAxis) - point2(embeddingAxis))
      if (d > distance) d else distance
    })
  }

}

case class Space(embeddingAxes: ImmutableArray[Int], origin: TupleX, unitCubeSizes: TupleX) extends HyperSpace

