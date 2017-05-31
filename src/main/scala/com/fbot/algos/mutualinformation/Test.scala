package com.fbot.algos.mutualinformation

/**
  * Copyright (C) 5/29/2017 - REstore NV
  *
  */
object Test extends App {

  val tuples: TupleArray = TupleArray(
    Tuple(0,0), Tuple(1,0), Tuple(1,1), Tuple(-1, 2), Tuple(-2,-3),
    Tuple(5,1), Tuple(1,5), Tuple(-5,1), Tuple(1,-5),
    Tuple(5,5), Tuple(-5,5), Tuple(-5,-5), Tuple(5,-5),
    Tuple(6,5), Tuple(7,6))

  val k = 2
  val centerTupleIndex = 0

  println(s"All tuples:\n$tuples")
  println(s"Center tuple:\n${ tuples(centerTupleIndex) }")
  val kNearestIndices = tuples.nearest(k, centerTupleIndex)
  val kNearest = TupleArray(kNearestIndices.map(tuples(_)))
  println(s"$k-Nearest:\n$kNearest")
}
