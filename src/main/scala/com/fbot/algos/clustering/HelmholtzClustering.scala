package com.fbot.algos.clustering

import org.apache.spark.SparkContext
import org.apache.spark.mllib.linalg.distributed.BlockMatrix

/**
  *
  */
class HelmholtzClustering {

  val x: ColMatrix = ???
}


case class ColMatrix(cols: BlockMatrix) {

  def sumRow: BlockMatrix = {
    ???
  }

}

object ColMatrix {

}
