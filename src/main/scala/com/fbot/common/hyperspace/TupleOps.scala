package com.fbot.common.hyperspace

/**
  *
  */
object TupleOps {

  def distance(a: Tuple, b: Tuple): Double = {
    val dim = a.dim

    var distance: Double = 0d
    var i: Int = 0
    while (i < dim) {
      val d = math.abs(a(i) - b(i))
      if (d > distance) distance = d
      i += 1
    }

    distance
  }

}
