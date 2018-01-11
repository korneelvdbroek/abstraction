package com.fbot.common.timeseries

import java.time.{Instant, ZonedDateTime}

import com.fbot.common.fastcollections.FastArray2Zipped._
import com.fbot.common.fastcollections.ImmutableArray
import com.fbot.common.fastcollections.ImmutableArray._

import scala.reflect.ClassTag

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
  */
case class TimeSeries(timestamps: ImmutableArray[Long], values: ImmutableArray[Double]) {

  require(timestamps.length == values.length, s"Number of timestamps does not match number of values in TimeSeries")

  def length: Int = timestamps.length

  def head: (Instant, Double) = (instantFromLong(timestamps.head), values.head)

  def groupBy[Key: ClassTag](f: (Instant, Double) => Key): ImmutableArray[TimeSeries] = {
    ImmutableArray((timestamps, values)
                     .groupBy((t, x) => f(instantFromLong(t), x))
                     .values
                     .map(timestampsAndValues => TimeSeries(timestampsAndValues._1, timestampsAndValues._2)))
  }

  def fragmentBy(start: (Instant, Double) => Boolean,
                 end: ((Instant, Double), (Instant, Double)) => Boolean): ImmutableArray[TimeSeries] = {
    val indicesOfStart = (timestamps, values).indicesWhere((t, x) => start(instantFromLong(t), x))

    indicesOfStart.map(startIndex => {
      val startDataPoint = (instantFromLong(timestamps(startIndex)), values(startIndex))
      val (slicedTimestamps, slicedValues) = (timestamps, values).sliceWhile((t, x) => end(startDataPoint, (instantFromLong(t), x)), startIndex).zipArray
      TimeSeries(slicedTimestamps, slicedValues)
    })
  }

  private def instantFromLong(seconds: Long): Instant = Instant.ofEpochSecond(seconds)

}

object TimeSeries {

  def apply(data: TraversableOnce[(ZonedDateTime, Double)]): TimeSeries = {
    val timestamps = data.map(dateTime => {
      dateTime._1.toInstant.getEpochSecond
    })
    val values = data.map(dateTime => {
      dateTime._2
    })

    new TimeSeries(ImmutableArray(timestamps), ImmutableArray(values))
  }

  def fromCsv(fileName: String,
              separator: String = ",", skipHeaderLines: Int = 1)
             (timestampedValueFromRow: ImmutableArray[String] => (ZonedDateTime, Double)): TimeSeries = {
    TimeSeries(ImmutableArray.fromCsv("data/UK_grid_power/DemandData_2011-2016.csv")(timestampedValueFromRow).toWrappedArray)
  }

}
