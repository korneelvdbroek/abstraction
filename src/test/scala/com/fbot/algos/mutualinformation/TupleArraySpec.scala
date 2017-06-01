package com.fbot.algos.mutualinformation

import org.scalatest.{FlatSpec, Matchers}

/**
  * Copyright (C) 5/30/2017 - REstore NV
  *
  */
class TupleArraySpec extends FlatSpec with Matchers {

  import TupleArraySpec._

  "sortedData" should "return the indices of the TupleArray sorted along the 0th axes" in {
    UnsortedTupleCloud.sortedData(0)._1 shouldBe Array(5, 0, 1, 2, 4, 3)
    UnsortedTupleCloud.sortedData(0)._2 shouldBe Array(1, 2, 3, 5, 4, 0)
  }

  it should "return the indices of the TupleArray sorted along the 1st axes" in {
    UnsortedTupleCloud.sortedData(1)._1 shouldBe Array(5, 3, 4, 1, 2, 0)
    UnsortedTupleCloud.sortedData(1)._2 shouldBe Array(5, 3, 4, 1, 2, 0)
  }

  "dim" should "return 2 for a TupleArray with 2 dimensional Tuples" in {
    UnsortedTuples.dim shouldBe 2
  }

  it should "return 0 when TupleArray is empty" in {
    emptyTuples.dim shouldBe 0
  }

}

object TupleArraySpec {

  val UnsortedTuples = TupleArray(Tuple(1, 5), Tuple(2, 3), Tuple(3, 4), Tuple(5, 1), Tuple(4, 2), Tuple(0, 0))
  val UnsortedTupleCloud = TupleCloud(UnsortedTuples)

  val emptyTuples = TupleArray()

}
