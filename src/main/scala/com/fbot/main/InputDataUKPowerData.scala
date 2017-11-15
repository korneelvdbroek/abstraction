package com.fbot.main

import java.time.format.DateTimeFormatter
import java.time.{LocalDateTime, ZoneId, ZonedDateTime}

import com.fbot.common.data.MultiSeries
import com.fbot.common.fastcollections.ImmutableArray
import com.fbot.common.fastcollections.index.ArrayIndex
import com.fbot.common.hyperspace.Tuple
import com.fbot.common.timeseries.TimeSeries
import org.apache.spark.SparkContext

/**
  * Data file can be found on
  *   http://www2.nationalgrid.com/UK/Industry-information/Electricity-transmission-operational-data/Data-Explorer/
  */
case class InputDataUKPowerData(implicit sc: SparkContext) extends TestData {

  def data: MultiSeries = {
    // timestamps
    val formatter = DateTimeFormatter.ofPattern("dd-MMM-yyyy HH:mm:ss")
    val timeZone = ZoneId.of("Europe/London")
    val timestamps = ImmutableArray.fromCsv("data/UK_grid_power/DemandData_2011-2016.csv")(row => {
      val date = ZonedDateTime.of(LocalDateTime.parse(row(ArrayIndex(0)) + " 00:00:00", formatter), timeZone)
      val minutes = (row(ArrayIndex(1)).toLong - 1L) * 30L
      val dateTime = date.plusMinutes(minutes)
      dateTime.toInstant.getEpochSecond
    })

    // power data
    val powerSeries = ImmutableArray.fromCsv("data/UK_grid_power/DemandData_2011-2016.csv")(row => {
      row(ArrayIndex(2)).toDouble
    })

    val powerTimeSeries = TimeSeries(timestamps, powerSeries)

    // make time series fragments
    val fragments = powerSeries.toArray.toIterator.sliding(48, 48).withPartial(false).map(ImmutableArray(_).map(Tuple(_)))

    MultiSeries(fragments)
  }

}
