package com.fbot.common.fastcollections

import com.fbot.common.fastcollections.FastArray2Zipped._
import org.scalatest.{FlatSpec, Matchers}

/**
  *
  */
class SpecFastArray2Zipped extends FlatSpec with Matchers {

  import SpecFastArray2Zipped._

  "sliceWhile" should "return a slice of the original tuple series" in {
    (seriesLeft1, seriesRight1).sliceWhile((left, _) => left <= 2, 1).zipArray shouldBe (ImmutableArray(1, 2), ImmutableArray(11, 12))
  }

  it should "return a slice of the original tuple series all the way to the end" in {
    (seriesLeft1, seriesRight1).sliceWhile((_, _) => true, 1).zipArray shouldBe (ImmutableArray(1, 2, 3), ImmutableArray(11, 12, 13))
  }

  it should "return an empty slice of the original tuple series" in {
    (seriesLeft1, seriesRight1).sliceWhile((_, _) => false, 1).zipArray shouldBe (ImmutableArray.empty[Int], ImmutableArray.empty[Int])
  }

}

object SpecFastArray2Zipped {

  val seriesLeft1 = ImmutableArray(0, 1, 2, 3)
  val seriesRight1 = ImmutableArray(10, 11, 12, 13)
}
