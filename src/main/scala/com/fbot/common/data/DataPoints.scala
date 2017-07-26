package com.fbot.common.data

import com.fbot.common.fastcollections.ImmutableArray
import com.fbot.common.hyperspace.{HyperSpace, Tuple}

/**
  *
  */
trait DataPoints {

  val space: HyperSpace
  val points: ImmutableArray[Tuple]

}