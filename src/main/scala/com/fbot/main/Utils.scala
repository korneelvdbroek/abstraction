package com.fbot.main

import com.fbot.common.fastcollections.ImmutableArray
import com.fbot.common.fastcollections.ImmutableArray._

/**
  *
  */
object Utils {

  def timeIt[R](block: => R): (R, Long) = {
    val t0 = System.nanoTime()
    val result = block
    // call-by-name
    val t1 = System.nanoTime()
    (result, t1 - t0)
  }

  def prettyPrintTime(i: Long): String = {
    val ms = i / 1000000
    val us = (i / 1000) % 1000
    val ns = i % 1000
    f"$ms%6d ms $us%3d us $ns%3d ns"
  }

  def printImmutableMatrix[T](matrix: ImmutableArray[ImmutableArray[T]],
                              formatPattern: String = "%5.3f", separator: String = ", ",
                              maxRows: Option[Int] = None, maxCols: Option[Int] = Some(10)): String = {
    val lines = matrix.mapWithIndex((row, rowIndex) => {
      if (maxRows.forall(rowIndex.toInt < _)) {
        val rowStr = row.mapWithIndex((value, colIndex) => {
          if (maxCols.forall(colIndex.toInt < _)) {
            formatPattern.format(value) + (if (colIndex.toInt < row.length - 1) separator else "")
          } else if (maxCols.contains(colIndex.toInt)) {
            "..."
          } else {
            ""
          }
        })

        rowStr.foldLeft("")((acc, entry) => acc + entry) + "\n"
      } else if (maxRows.contains(rowIndex.toInt)) {
        "..."
      } else {
        ""
      }
    })

    lines.foldLeft("")((acc, line) => acc + line)
  }

}