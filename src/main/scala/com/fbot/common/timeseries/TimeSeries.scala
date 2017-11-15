package com.fbot.common.timeseries

import java.time.ZonedDateTime

import com.fbot.common.fastcollections.ImmutableArray

/**
  *
  */
case class TimeSeries(timestamps: ImmutableArray[Long], values: ImmutableArray[Double])

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

}
