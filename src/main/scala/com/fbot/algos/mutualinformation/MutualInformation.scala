package com.fbot.algos.mutualinformation

import com.fbot.common.fastcollections.ImmutableArray
import com.fbot.common.hyperspace.Tuple
import breeze.numerics.digamma

/**
  * References:
  * + Estimating Mutual Information
  *   Alexander Kraskov, Harald Stogbauer, Peter Grassberger
  *   Phys. Rev. E 69, 066138
  *  	arXiv:cond-mat/0305641
  */
case class MutualInformation(dataX: ImmutableArray[Tuple], dataY: ImmutableArray[Tuple]) {
  val x: Double = 7d
  println(digamma(x))
  /**
    * TODO:
    * + generalize numberOfCloseByPointsBruteForce to work on subspaces
    */
}
