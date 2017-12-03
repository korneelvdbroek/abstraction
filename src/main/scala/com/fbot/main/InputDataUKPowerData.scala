package com.fbot.main

import java.time.format.DateTimeFormatter
import java.time.temporal.ChronoField
import java.time.{Instant, LocalDateTime, ZoneId, ZonedDateTime}

import com.fbot.common.data.MultiSeries
import com.fbot.common.fastcollections.ImmutableArray
import com.fbot.common.fastcollections.ImmutableArray._
import com.fbot.common.fastcollections.index.ArrayIndex
import com.fbot.common.hyperspace.Tuple
import com.fbot.common.timeseries.TimeSeries
import grizzled.slf4j.Logging
import org.apache.spark.SparkContext

/**
  * Data file can be found on
  * http://www2.nationalgrid.com/UK/Industry-information/Electricity-transmission-operational-data/Data-Explorer/
  */
case class InputDataUKPowerData(implicit sc: SparkContext) extends TestData with Logging {

  def data: MultiSeries = {

    val timeZone = ZoneId.of("Europe/London")
    val csvFormatter = DateTimeFormatter.ofPattern("dd-MMM-yyyy HH:mm:ss")

    val powerTimeSeries = TimeSeries.fromCsv("data/UK_grid_power/DemandData_2011-2016.csv")(row => {
      val date = ZonedDateTime.of(LocalDateTime.parse(row(ArrayIndex(0)) + " 00:00:00", csvFormatter), timeZone)
      val minutes = (row(ArrayIndex(1)).toLong - 1L) * 30L
      val dateTime = date.plusMinutes(minutes)

      val value = row(ArrayIndex(2)).toDouble
      (dateTime, value)
    })

    // make time series fragments
//    val fragments: ImmutableArray[TimeSeries] = powerTimeSeries.groupBy((t, _) => {
//      ZonedDateTime.ofInstant(t, timeZone).toLocalDate //.`with`(ChronoField.DAY_OF_WEEK, 1L)
//    })
    val fragments: ImmutableArray[TimeSeries] = powerTimeSeries.fragmentBy((t, _) => {
      ZonedDateTime.ofInstant(t, timeZone).toLocalDateTime.get(ChronoField.MINUTE_OF_DAY) < 1
    }, (start, end) => ZonedDateTime.ofInstant(start._1, timeZone).toLocalDate.plusDays(3).isAfter(ZonedDateTime.ofInstant(end._1, timeZone).toLocalDate))

    val seriesData = MultiSeries(fragments.filter(timeSeries => {
      timeSeries.length == 48 * 3 && timeSeries.head._1.isAfter(Instant.parse("2015-12-31T23:59:59Z"))
    }).map(_.values.map(Tuple(_))))

    info(s"Number of data fragments = ${seriesData.length }")
    info(s"Length of each data fragment = ${seriesData(0).length }")

    seriesData
  }

}
