package com.fbot.main

import com.fbot.common.data.MultiSeries
import com.fbot.common.fastcollections.ImmutableArray
import com.fbot.common.fastcollections.index.ArrayIndex
import com.fbot.common.hyperspace.Tuple
import org.apache.spark.SparkContext

import scala.io.Source
import scala.util.Try

/**
  *
  */
case class InputDataYeast(implicit sc: SparkContext) extends TestData {

  def data: MultiSeries = {
    val bufferedSource = Source.fromFile("C:/Users/Korneel/Korneel_local/program/abstraction/doc/yeast/complete_dataset_part.txt")

    val dataSeries = ImmutableArray(bufferedSource.getLines.map(line => {
      val cells = line.split("\t", -1).toList
      val dataCells = cells.slice(3, cells.length)
      ImmutableArray(dataCells.map(dataCell => {
        Tuple(Try(dataCell.toDouble).getOrElse(0.0))
      }))
    }))
    val dataSeriesNoHeader = dataSeries.slice(1, dataSeries.length)
    bufferedSource.close

    println(dataSeriesNoHeader(ArrayIndex(0)))

    // validation on length of series
    val expectedLength = dataSeriesNoHeader(ArrayIndex(0)).length
    dataSeriesNoHeader.map(_.length).toList.zipWithIndex.foreach (x => {
      val (len, line) = x
      if (len != expectedLength) {
        println(s"issue on line $line: len = $len != $expectedLength")
      }
    })

    MultiSeries(dataSeriesNoHeader)
  }

}

