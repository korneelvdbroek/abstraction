package com.fbot.algos.mutualinformation

import com.fbot.common.fastcollections.ImmutableTupleArray

/**
  * Created by filip on 16/09/2017.
  */
case class DelayIndependentMutualInformation(dataX: ImmutableTupleArray, dataY: ImmutableTupleArray, maxDelay: Int) {

  def MI(k: Int): Double = {
    (-maxDelay to maxDelay).map(calculateDelayedMI(_, k)).max
  }

  def calculateDelayedMI(delay: Int, k: Int): Double = {
    val partOfX = dataX.slice(maxDelay, dataX.length - maxDelay)
    val partOfY = dataX.slice(maxDelay + delay, dataX.length - maxDelay + delay)

    MutualInformation(partOfX, partOfY).MI(k)
  }
}
