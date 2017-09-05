package com.fbot.common.data

import com.fbot.common.fastcollections.ImmutableArray
import com.fbot.common.fastcollections.index.ArrayIndex
import com.fbot.common.hyperspace.Tuple
import org.apache.spark.SparkContext
import org.apache.spark.rdd.RDD

/**
  *
  */
case class BigData(series: RDD[(ArrayIndex, ImmutableArray[Tuple])]) {

  def apply(i: ArrayIndex): ImmutableArray[Tuple] = series.lookup(i).head

  def apply(i: => Int): ImmutableArray[Tuple] = apply(ArrayIndex(i))

}

object BigData {

  def apply(series: ImmutableArray[Tuple]*)(implicit sc: SparkContext): BigData = {
    val matrix = series.toArray.mapWithIndex((row, index) => (index, row)).toArray
    BigData(sc.parallelize(matrix))
  }

}
