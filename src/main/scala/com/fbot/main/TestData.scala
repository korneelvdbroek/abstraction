package com.fbot.main

import breeze.linalg.{DenseMatrix, DenseVector}
import breeze.stats.distributions.MultivariateGaussian
import com.fbot.algos.mutualinformation.MIData
import com.fbot.common.fastcollections.ImmutableArray
import com.fbot.common.fastcollections.index.ArrayIndex
import com.fbot.common.hyperspace.{Space, Tuple}

import scala.util.Random

/**
  * Copyright (C) 8/16/2017 - REstore NV
  *
  */
trait TestData {

  def data: MIData

}

case class RndDataXd(dim: Int, N: Int) extends TestData {

  def data: MIData = {
    def randomDouble = {
      val sign = if (Random.nextBoolean()) 1 else -1
      sign * Random.nextInt(1000) * 1d
    }

    val dataX = ImmutableArray(Array.fill[Tuple](N)(Tuple(Array.fill(dim)(randomDouble))))
    val dataY = ImmutableArray(Array.fill[Tuple](N)(Tuple(Array.fill(dim)(randomDouble))))

    // 0 - 1,000   of 1,000,000 = 10^6
    // volume total space = 2000^8 = 2^8 10^(3*8)   = 256   10^24  --> 10^6  points
    // volume unit cube   =  500^8 = 1/2^8 10^(3*8) = 1/256 10^24  --> 15.25 points
    MIData(dataX, dataY)
  }

}


case class FxDataXd(dim: Int, N: Int) extends TestData {

  def data: MIData = {
    def randomDouble = {
      val sign = if (Random.nextBoolean()) 1 else -1
      sign * Random.nextInt(1000) * 1d
    }
    def f(tuple: Tuple): Tuple = {
      Tuple(tuple.repr.map(2d * _))
    }

    val dataX = ImmutableArray(Array.fill[Tuple](N)(Tuple(Array.fill(dim)(randomDouble))))
    val dataY = dataX.map(f)

    // 0 - 1,000   of 1,000,000 = 10^6
    // volume total space = 2000^8 = 2^8 10^(3*8)   = 256   10^24  --> 10^6  points
    // volume unit cube   =  500^8 = 1/2^8 10^(3*8) = 1/256 10^24  --> 15.25 points
    MIData(dataX, dataY)
  }

}

case class GaussianData2d(N: Int, r: Double) extends TestData {

  def data: MIData = {
    val mu = DenseVector(0d, 0d)
    val sigma = DenseMatrix((1d, r),
                            (r, 1d))
    val gaussian = MultivariateGaussian(mu, sigma)

    val sample = ImmutableArray.fill[Tuple](N)(Tuple(gaussian.draw().toArray))
    val dataX = sample.map(tuple => Tuple(tuple(0)))
    val dataY = sample.map(tuple => Tuple(tuple(1)))

    // 0 - 1,000   of 1,000,000 = 10^6
    // volume total space = 2000^8 = 2^8 10^(3*8)   = 256   10^24  --> 10^6  points
    // volume unit cube   =  500^8 = 1/2^8 10^(3*8) = 1/256 10^24  --> 15.25 points
    MIData(dataX, dataY)
  }

}

object Data2d extends TestData {

  def data: MIData = {
    //  5432101234567
    //
    //              4 6
    //  0     6   93  5
    //                4
    //                3
    //      3         2
    //  7     2   5   1
    //       01       0
    //                1
    //                2
    //     4          3
    //                4
    //  1     8   2   5
    //
    val space = Space(ImmutableArray.indexRange(0, 2), ImmutableArray(6.0, 6.0))
    val data = ImmutableArray(
      Tuple(0, 0),
      Tuple(1, 0), Tuple(1, 1), Tuple(-1, 2), Tuple(-2, -3),
      Tuple(5, 1), Tuple(1, 5), Tuple(-5, 1), Tuple(1, -5),
      Tuple(5, 5), Tuple(-5, 5), Tuple(-5, -5), Tuple(5, -5),
      Tuple(6, 5), Tuple(7, 6))

    val centerTupleIndex = ArrayIndex(9)

    MIData(data, data)
  }
}