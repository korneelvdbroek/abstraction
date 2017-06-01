package com.fbot.algos.mutualinformation

/**
  * Copyright (C) 5/31/2017 - REstore NV
  *
  */
case class HyperCubeSide(axis: Int, direction: Boolean) {

  override def toString: String = {
    val sign = if (direction) "+" else "-"
    s"${sign}x_$axis"
  }

}
