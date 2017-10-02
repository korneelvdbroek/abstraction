package com.fbot.algos.clustering

import com.fbot.common.fastcollections.ImmutableDoubleArray
import org.apache.spark.rdd.RDD

/**
  *
  */
class HelmholtzClustering {

  val x: ColMatrix = ???
  x.cols.treeReduce((x, y) => x + y)
}


case class ColMatrix(cols: RDD[ImmutableDoubleArray])
