package com.fbot.common.data

import com.fbot.common.fastcollections.ImmutableArray
import com.fbot.common.fastcollections.ImmutableArray._
import com.fbot.common.fastcollections.Tuple
import grizzled.slf4j.Logging
import org.apache.spark.rdd.RDD
import org.apache.spark.{HashPartitioner, SparkContext}

import scala.reflect.ClassTag

/**
  * Copyright (C) 2017-2018  korneelvdbroek
  *
  * This program is free software: you can redistribute it and/or modify
  * it under the terms of the GNU General Public License as published by
  * the Free Software Foundation, either version 3 of the License, or
  * (at your option) any later version.
  *
  * This program is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU General Public License for more details.
  *
  * You should have received a copy of the GNU General Public License
  * along with this program.  If not, see <http://www.gnu.org/licenses/>.
  *
  */
// TODO: convert this to a RDD[Tuple] or, even better, an RDD[ImmutableArray[Tuple]]
case class Series(data: ImmutableArray[Tuple]) {

  def toImmutableArray: ImmutableArray[Tuple] = data

  def length: Int = data.length

}

case class IndexedSeries(index: Long, series: Series) {

  def length: Int = series.length

  def data: ImmutableArray[Tuple] = series.toImmutableArray

}

object IndexedSeries {

  def apply(seriesIndexAndSeries: (Long, ImmutableArray[Tuple])): IndexedSeries = IndexedSeries(seriesIndexAndSeries._1, Series(seriesIndexAndSeries._2))

  def apply(seriesIndex: Long, series: ImmutableArray[Tuple]): IndexedSeries = IndexedSeries(seriesIndex, Series(series))

}


case class MultiSeries(series: RDD[(Long, ImmutableArray[Tuple])], length: Int) extends Logging {

  def apply(i: Long): ImmutableArray[Tuple] = series.lookup(i).head

  def map[U](f: (IndexedSeries) ⇒ U)(implicit arg0: ClassTag[U]): RDD[U] = {
    series.map(x => f(IndexedSeries(x)))
  }

  def flatMap[U](f: IndexedSeries ⇒ TraversableOnce[U])(implicit classTag: ClassTag[U]): RDD[U] = {
    series.flatMap(x => f(IndexedSeries(x)))
  }

  def cartesian(other: MultiSeries): RDD[(IndexedSeries, IndexedSeries)] = {
    series.cartesian(other.series).map(seriesPair => (IndexedSeries(seriesPair._1), IndexedSeries(seriesPair._2)))
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
    val matrix = series.mapWithIndex((row, index) => (index.toLong, row)).toArray

    // https://stackoverflow.com/questions/40636554/spark-ui-dag-stage-disconnected
    // .partitionBy right after parallelization is still advantageous since we partition by the row index
    val rdd = sc.parallelize(matrix).partitionBy(new HashPartitioner(sc.defaultParallelism * 4)).cache()

    new MultiSeries(rdd, series.length)
  }

  case class SeriesIndexCombination(combination: ImmutableArray[Long], partitionIndex: Int) {

    def apply(pairIndex: Int): Long = combination(pairIndex)
  }

  type SeriesCombination = Vector[IndexedSeries]

}
