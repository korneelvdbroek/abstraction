package com.fbot.common.fastcollections

import com.fbot.common.fastcollections.fastarrayops.FastArray2Zipped._
import com.fbot.common.fastcollections.deprecated.ImmutableArrayY
import org.scalatest.{FlatSpec, Matchers}

/**
  *
  */
class SpecFastArray2Zipped extends FlatSpec with Matchers {

  import SpecFastArray2Zipped._

  "sliceWhile" should "return a slice of the original tuple series" in {
    (seriesLeft1, seriesRight1).sliceWhile((left, _) => left <= 2, 1).zipArray shouldBe (ImmutableArrayY(1, 2), ImmutableArrayY(11, 12))
  }

  it should "return a slice of the original tuple series all the way to the end" in {
    (seriesLeft1, seriesRight1).sliceWhile((_, _) => true, 1).zipArray shouldBe (ImmutableArrayY(1, 2, 3), ImmutableArrayY(11, 12, 13))
  }

  it should "return an empty slice of the original tuple series" in {
    (seriesLeft1, seriesRight1).sliceWhile((_, _) => false, 1).zipArray shouldBe (ImmutableArrayY.empty[Int], ImmutableArrayY.empty[Int])
  }

}

object SpecFastArray2Zipped {

  val seriesLeft1 = ImmutableArrayY(0, 1, 2, 3)
  val seriesRight1 = ImmutableArrayY(10, 11, 12, 13)
}
