package com.fbot.common.fastcollections

import com.fbot.common.fastcollections.fastarrayops.FastArray2Zipped._
import org.scalatest.{FlatSpec, Matchers}

/**
  *
  */
class SpecFastArray2Zipped extends FlatSpec with Matchers {

  import SpecFastArray2Zipped._

  "sliceWhile" should "return a slice of the original tuple series" in {
    val (left, right) = (seriesLeft1, seriesRight1).sliceWhile((left, _) => left <= 2, ArrayIndex(1)).zipArray
    left.toList shouldBe ImmutableArray(1, 2).toList
    right.toList shouldBe ImmutableArray(11, 12).toList
  }

  it should "return a slice of the original tuple series all the way to the end" in {
    val (left, right) = (seriesLeft1, seriesRight1).sliceWhile((_, _) => true, ArrayIndex(1)).zipArray
    left.toList shouldBe ImmutableArray(1, 2, 3).toList
    right.toList shouldBe ImmutableArray(11, 12, 13).toList
  }

  it should "return an empty slice of the original tuple series" in {
    val (left, right) = (seriesLeft1, seriesRight1).sliceWhile((_, _) => false, ArrayIndex(1)).zipArray
    left.toList shouldBe ImmutableArray.empty[Int].toList
    right.toList shouldBe ImmutableArray.empty[Int].toList
  }

}

object SpecFastArray2Zipped {

  val seriesLeft1 = ImmutableArray(0, 1, 2, 3)
  val seriesRight1 = ImmutableArray(10, 11, 12, 13)
}
