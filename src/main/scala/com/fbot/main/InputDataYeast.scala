package com.fbot.main

import com.fbot.common.data.MultiSeries
import com.fbot.common.fastcollections.ImmutableArray
import com.fbot.common.fastcollections._
import com.fbot.common.fastcollections.Tuple
import org.apache.spark.SparkContext

import scala.io.Source
import scala.util.Try

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
case class InputDataYeast(implicit sc: SparkContext) extends TestData {

  def data: MultiSeries = {
    val bufferedSource = Source.fromFile("doc/yeast/figure3_173microarray_colcleaned.cdt.txt")

    val dataSeries = ImmutableArray(bufferedSource.getLines.map(line => {
      val cells = line.split("\t", -1).toList
      val dataCells = cells.slice(7, cells.length)
      ImmutableArray(dataCells.map(dataCell => {
        Tuple(Try(dataCell.toDouble).getOrElse(0.0))
      }))
    }))
    val dataSeriesNoHeader: ImmutableArray[ImmutableArray[Tuple]] = dataSeries.slice(1, dataSeries.length)
    bufferedSource.close

    println(dataSeriesNoHeader(ArrayIndex(0)))
    println(dataSeriesNoHeader(ArrayIndex(1)))
    println(dataSeriesNoHeader(ArrayIndex(2)))

    // validation on length of series
    val expectedLength = dataSeriesNoHeader(ArrayIndex(0)).length
    dataSeriesNoHeader.mapToNewType(_.length).toList.zipWithIndex.foreach(x => {
      val (len, line) = x
      if (len != expectedLength) {
        println(s"issue on line $line: len = $len != $expectedLength")
      }
    })

    MultiSeries(dataSeriesNoHeader.toArray.map(ImmutableTupleArray.fromTuples))
  }

}

