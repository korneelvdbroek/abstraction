package com.fbot.algos.mutualinformation

import com.fbot.algos.nearestneighbors.NearestNeighbors
import com.fbot.common.fastcollections.FastTuple2Zipped._
import com.fbot.common.fastcollections.ImmutableArray
import com.fbot.common.fastcollections.index.ArrayIndex
import com.fbot.common.hyperspace.{HyperSpace, HyperSpaceUnit, Space, Tuple}

/**
  *
  */
case class MIData(dataX: ImmutableArray[Tuple], dataY: ImmutableArray[Tuple]) extends NearestNeighbors {

  implicit def builder(array: Array[Tuple]): ImmutableArray[Tuple] = ImmutableArray(array)
  val points: ImmutableArray[Tuple] = tuple2FastTuple2Zipped(dataX, dataY).map((x, y) => x ++ y)


  val dim: Int = dataX(ArrayIndex(0)).dim

  val unitSize = 500d
  val unitSizeProjected = 100d

  val space: HyperSpace = Space(ImmutableArray.indexRange(0, 2*dim), ImmutableArray.fill(2*dim)(unitSize))
  val spaceX: HyperSpace = Space(ImmutableArray.indexRange(0, dim), ImmutableArray.fill(dim)(unitSizeProjected))
  val spaceY: HyperSpace = Space(ImmutableArray.indexRange(dim, 2*dim), ImmutableArray.fill(dim)(unitSizeProjected))


  lazy val pointsBySpaceUnitPerSpace: Map[HyperSpace, Map[HyperSpaceUnit, ImmutableArray[ArrayIndex]]] = {
    // Slow initial computation
    val pointsBySpaceUnit: Map[HyperSpaceUnit, ImmutableArray[ArrayIndex]] =
      points.indexRange.groupBy(index => space.hyperSpaceUnitAround(points(index)))

    //    val spaceUnits = ImmutableArray(pointsBySpaceUnit.keys)
    //
    //    val pointsByProjectedSpaceXUnits: Map[HyperSpaceUnit, ImmutableArray[ArrayIndex]] =
    //      spaceUnits.groupBy(_.project(spaceX)).mapValues(_.flatMap(pointsBySpaceUnit(_).toArray))
    //
    //    val pointsByProjectedSpaceYUnits: Map[HyperSpaceUnit, ImmutableArray[ArrayIndex]] =
    //      spaceUnits.groupBy(_.project(spaceY)).mapValues(_.flatMap(pointsBySpaceUnit(_).toArray))

    val pointsByProjectedSpaceXUnits: Map[HyperSpaceUnit, ImmutableArray[ArrayIndex]] =
      points.indexRange.groupBy(index => spaceX.hyperSpaceUnitAround(points(index)))

    val pointsByProjectedSpaceYUnits: Map[HyperSpaceUnit, ImmutableArray[ArrayIndex]] =
      points.indexRange.groupBy(index => spaceY.hyperSpaceUnitAround(points(index)))

    Map(space -> pointsBySpaceUnit,
        spaceX -> pointsByProjectedSpaceXUnits,
        spaceY -> pointsByProjectedSpaceYUnits)
  }

}