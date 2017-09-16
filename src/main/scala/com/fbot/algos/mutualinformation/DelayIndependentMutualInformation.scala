package com.fbot.algos.mutualinformation

import com.fbot.common.fastcollections.ImmutableArray
import com.fbot.common.fastcollections.index.ArrayIndex
import com.fbot.common.hyperspace.Tuple

/**
  * Created by filip on 16/09/2017.
  */
case class DelayIndependentMutualInformation(dataX: ImmutableArray[Tuple], dataY: ImmutableArray[Tuple], maxDelay: Int) {

  def MI(k: Int): Double = {
    (-maxDelay to maxDelay).map(calculateDelayedMI(_, k)).max
  }

  def calculateDelayedMI(delay: Int, k: Int): Double = {
    val partOfX = ImmutableArray(dataX.repr.slice(maxDelay, dataX.length - maxDelay))
    val partOfY = ImmutableArray(dataX.repr.slice(maxDelay + delay, dataX.length - maxDelay + delay))

    MutualInformation(partOfX, partOfY).MI(k)
  }
}
