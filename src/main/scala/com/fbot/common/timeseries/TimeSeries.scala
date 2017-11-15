package com.fbot.common.timeseries

import java.time.{Instant, ZonedDateTime}

import com.fbot.common.fastcollections.ImmutableArray
import com.fbot.common.fastcollections.FastTuple2Zipped._

import scala.reflect.ClassTag

/**
  *
  */
case class TimeSeries(timestamps: ImmutableArray[Long], values: ImmutableArray[Double]) {

  require(timestamps.length == values.length, s"Number of timestamps does not match number of values in TimeSeries")

  def length: Int = timestamps.length

  def groupByTimestamps[Key: ClassTag](f: (Instant, Double) => Key): ImmutableArray[TimeSeries] = {
    //TODO: need to define groupBy on 2Zipped
    (timestamps, values).groupBy(t => f(instantFromLong(t))).values

    ???
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
