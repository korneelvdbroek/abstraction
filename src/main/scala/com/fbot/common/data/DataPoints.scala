package com.fbot.common.data

import com.fbot.common.fastcollections.ImmutableArray
import com.fbot.common.hyperspace.{HyperSpace, Tuple}

/**
  * Copyright (C) 7/24/2017 - REstore NV
  *
  */
trait DataPoints {

  val space: HyperSpace
  val points: ImmutableArray[Tuple]

}