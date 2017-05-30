package com.fbot.algos.mutualinformation

/**
  * Copyright (C) 5/29/2017 - REstore NV
  *
  */
object Test extends App {

  val tuples: TupleArray = TupleArray(Tuple(1,0,0), Tuple(2,1,0), Tuple(0,0,1), Tuple(5, 5, 5))

  println(tuples)
  tuples.nearest(2)
}
