package com.fbot.common

import java.time.{ZoneId, ZonedDateTime}

import com.fbot.common.fastcollections.{ImmutableArray, ImmutableArraySpec}
import com.fbot.common.timeseries.TimeSeries
import org.scalatest.{FlatSpec, Matchers}

/**
  *
  */
class SpecTimeSeries extends FlatSpec with Matchers {

  import SpecTimeSeries._

  "fragmentBy" should "return 2 timeseries each containing 2 data points" in {
    val fragments = timeSeries1.fragmentBy((t, _) => ZonedDateTime.ofInstant(t, timeZone).getHour % 2 == 0,
                                           (start, end) => end._1.minusSeconds(3600 + 1).isBefore(start._1))
    fragments shouldBe ImmutableArray(TimeSeries(List((t0, 0d), (t0.plusHours(1), 1d))),
                                      TimeSeries(List((t0.plusHours(2), 4d), (t0.plusHours(3), 9d))))
  }

}

object SpecTimeSeries {

  val timeZone: ZoneId = ZoneId.of("Europe/Paris")

  val t0: ZonedDateTime = ZonedDateTime.parse("2013-10-13T10:11:12+01:00[Europe/Paris]")
  val timeSeries1 = TimeSeries(List((t0, 0d), (t0.plusHours(1), 1d), (t0.plusHours(2), 4d), (t0.plusHours(3), 9d)))

}
