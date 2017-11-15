package com.fbot.main

import breeze.linalg.{DenseMatrix => BDM, DenseVector => BDV, Matrix => BM}
import com.fbot.common.data.MultiSeries
import grizzled.slf4j.Logging
import org.apache.spark.SparkContext

/**
  *
  */
case class InputDataGaussian(implicit sc: SparkContext) extends TestData with Logging {

  def data: MultiSeries = {
    val rho: Double = 0.0d
    val Nsample: Int = 10000
    // should be > 10^{2 seriesDim}
    val seriesDim: Int = 2
    val N = 4

    val mu = BDV(Array.fill(N * seriesDim)(0d))
    val sigma = {
      val dim = 4

      val sigmaX = BDM.eye[Double](dim) + BDM((0.0, 0.0, 0.4, 0.4), (0.0, 0.0, 0.4, 0.4), (0.4, 0.4, 0.0, 0.0), (0.4, 0.4, 0.0, 0.0))
      val sigmaY = BDM.eye[Double](dim) + BDM((0.0, 0.0, 0.4, 0.4), (0.0, 0.0, 0.4, 0.4), (0.4, 0.4, 0.0, 0.0), (0.4, 0.4, 0.0, 0.0))
      val sigmaXY = BDM.eye[Double](dim) * rho //+ BDM((0.4, 0.0, -0.3, 0.0), (0.0, 0.0, 0.0, 0.0), (0.5, 0.0, 0.4, 0.0), (0.1, 0.0, 0.2, 0.0))

      val a = BDM.zeros[Double](2 * dim, 2 * dim)
      a(0 until dim, 0 until dim) := sigmaX
      a(dim until 2 * dim, dim until 2 * dim) := sigmaY
      a(0 until dim, dim until 2 * dim) := sigmaXY
      a(dim until 2 * dim, 0 until dim) := sigmaXY.t

      a
    }

    info(s"sigma = \n$sigma")
    info(s"parallelism = ${sc.defaultParallelism }")
    GaussianData(N, Nsample, seriesDim, sigma, mu).data // GaussianData.data // RndDataXd(dim, N).data // FxDataXd(dim, N).data // GaussianData2d(N, rho).data
  }

}
