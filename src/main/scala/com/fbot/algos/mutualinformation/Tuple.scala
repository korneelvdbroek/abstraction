package com.fbot.algos.mutualinformation

/**
  * Copyright (C) 5/30/2017 - REstore NV
  *
  */
case class Tuple(data: Array[Double]) extends AnyVal {

  def dim: Int = data.length

  def apply(i: Int): Double = data(i)

  override def toString: String = {
    data.mkString("(", ", ", ")")
  }

}

object Tuple {

  def apply(data: Double*): Tuple = new Tuple(data.toArray)

}

