package com.fbot.common.data

import com.fbot.common.data.MultiSeries.{SeriesCombination, SeriesIndexCombination}
import com.fbot.common.fastcollections.ImmutableArray
import com.fbot.common.fastcollections.index.ArrayIndex
import com.fbot.common.hyperspace.Tuple
import org.apache.spark.{HashPartitioner, SparkContext}
import org.apache.spark.rdd.RDD

import scala.reflect.ClassTag

/**
  *
  */
// TODO: convert this to a RDD[Tuple] or, even better, an RDD[ImmutableArray[Tuple]]
case class Series(data: ImmutableArray[Tuple]) {

  def toImmutableArray: ImmutableArray[Tuple] = data

}

case class IndexedSeries(index: ArrayIndex, series: Series)

object IndexedSeries {

  def apply(tuple: (ArrayIndex, ImmutableArray[Tuple])): IndexedSeries = IndexedSeries(tuple._1, Series(tuple._2))

  def apply(arrayIndex: ArrayIndex, series: ImmutableArray[Tuple]): IndexedSeries = IndexedSeries(arrayIndex, Series(series))

}


case class MultiSeries(series: RDD[(ArrayIndex, ImmutableArray[Tuple])], length: Int) {

  def apply(i: ArrayIndex): ImmutableArray[Tuple] = series.lookup(i).head

  def apply(i: => Int): ImmutableArray[Tuple] = apply(ArrayIndex(i))

  def map[U](f: (IndexedSeries) ⇒ U)(implicit arg0: ClassTag[U]): RDD[U] = {
    series.map(x => f(IndexedSeries(x)))
  }

  def flatMap[U](f: IndexedSeries ⇒ TraversableOnce[U])(implicit classTag: ClassTag[U]): RDD[U] = {
    series.flatMap(x => f(IndexedSeries(x)))
  }

  def makeSeriesPairs(pairings: Array[SeriesIndexCombination])(implicit sc: SparkContext): RDD[SeriesCombination] = {
    def joinOnSeries(acc: RDD[(Vector[IndexedSeries], SeriesIndexCombination)], pairIndex: Int): RDD[(Vector[IndexedSeries], SeriesIndexCombination)] = {
      acc
        .map(x => (x._2(pairIndex), x))
        .join(series)
        .map(x => (x._2._1._1 :+ IndexedSeries(x._1, x._2._2), x._2._1._2))
    }

    def partitionByIndex(acc: RDD[(Vector[IndexedSeries], SeriesIndexCombination)]): RDD[SeriesCombination] = {
      acc
        .map(x => (x._2.partitionIndex, x._1))
        .partitionBy(new HashPartitioner(sc.defaultParallelism))
        .map(x => x._2)
    }

    val pairingLength = pairings.headOption.map(_.combination.length)
    val z = flatMap(row => pairings.filter(_.combination.headOption.exists(_ == row.index)).map(pairIndex => (Vector(row), pairIndex)))
    val joined = (1 until pairingLength.getOrElse(1)).foldLeft(z)(joinOnSeries)

    partitionByIndex(joined)
  }



}

object MultiSeries {

  def apply(series: ImmutableArray[Tuple]*)(implicit sc: SparkContext): MultiSeries = {
    MultiSeries(series.toList)
  }

  def apply(series: TraversableOnce[ImmutableArray[Tuple]])(implicit sc: SparkContext): MultiSeries = {
    MultiSeries(ImmutableArray(series))
  }

  def apply(series: ImmutableArray[ImmutableArray[Tuple]])(implicit sc: SparkContext): MultiSeries = {
    val matrix = series.mapWithIndex((row, index) => (index, row)).toArray

    // https://stackoverflow.com/questions/40636554/spark-ui-dag-stage-disconnected
    // .partitionBy right after parallelization is still advantageous since we partition by the row index
    val rdd = sc.parallelize(matrix).partitionBy(new HashPartitioner(sc.defaultParallelism * 4)).cache()

    new MultiSeries(rdd, series.length)
  }

  case class SeriesIndexCombination(combination: Array[ArrayIndex], partitionIndex: Int) {
    def apply(pairIndex: Int): ArrayIndex = combination(pairIndex)
  }

  type SeriesCombination = Vector[IndexedSeries]

}
