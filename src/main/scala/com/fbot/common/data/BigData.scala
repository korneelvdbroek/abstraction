package com.fbot.common.data

import com.fbot.common.fastcollections.ImmutableArray
import com.fbot.common.fastcollections.index.ArrayIndex
import com.fbot.common.hyperspace.Tuple
import org.apache.spark.{HashPartitioner, SparkContext}
import org.apache.spark.rdd.RDD

import scala.reflect.ClassTag

/**
  *
  */
case class Row(index: ArrayIndex, rowData: ImmutableArray[Tuple]) {

  def toTuple: (ArrayIndex, ImmutableArray[Tuple]) = (index, rowData)

}

object Row {

  def apply(tuple: (ArrayIndex, ImmutableArray[Tuple])): Row = Row(tuple._1, tuple._2)

}


case class BigData(series: RDD[(ArrayIndex, ImmutableArray[Tuple])], rows: Int) {

  def apply(i: ArrayIndex): ImmutableArray[Tuple] = series.lookup(i).head

  def apply(i: => Int): ImmutableArray[Tuple] = apply(ArrayIndex(i))

  def flatMap[U](f: Row ⇒ TraversableOnce[U])(implicit arg0: ClassTag[U]): RDD[U] = {
    series.flatMap(x => f(Row(x)))
  }

}

object BigData {

  def apply(series: ImmutableArray[Tuple]*)(implicit sc: SparkContext): BigData = {
    val rows = series.toArray
    val matrix = rows.mapWithIndex((row, index) => (index, row)).toArray

    // https://stackoverflow.com/questions/40636554/spark-ui-dag-stage-disconnected
    // .partitionBy right after parallelization is still advantageous since we partition by the row index
    val rdd = sc.parallelize(matrix).partitionBy(new HashPartitioner(sc.defaultParallelism)).cache()

    BigData(rdd, rows.length)
  }

}
