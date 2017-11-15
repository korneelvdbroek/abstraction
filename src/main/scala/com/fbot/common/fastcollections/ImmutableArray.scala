package com.fbot.common.fastcollections

import java.time.format.DateTimeFormatter
import java.time.{LocalDateTime, ZoneId, ZonedDateTime}

import com.fbot.common.fastcollections.index.ArrayIndex

import scala.collection.mutable
import scala.io.Source
import scala.reflect.ClassTag
import scala.util.Try

/**
  *
  */
case class ImmutableArray[T](repr: mutable.WrappedArray[T]) extends AnyVal with FastArray[T, ImmutableArray] {

  def makeTransformed[B](x: mutable.WrappedArray[B]): ImmutableArray[B] = ImmutableArray(x)

}

object ImmutableArray {

  def apply[T: ClassTag](data0: T, dataRest: T*): ImmutableArray[T] = ImmutableArray[T]((data0 +: dataRest).toArray)

  def apply[T: ClassTag](data: TraversableOnce[T]): ImmutableArray[T] = ImmutableArray[T](data.toArray)

  def apply[T: ClassTag](data: Array[T]): ImmutableArray[T] = ImmutableArray[T](mutable.WrappedArray.make[T](data))

  def fill[T: ClassTag](n: Int)(elem: ⇒ T): ImmutableArray[T] = ImmutableArray(Array.fill[T](n)(elem))

  def empty[T: ClassTag]: ImmutableArray[T] = ImmutableArray(Array.empty[T])

  def range(start: Int, end: Int): ImmutableArray[Int] = ImmutableArray(Array.range(start, end))

  def indexRange(start: Int, end: Int): ImmutableArray[ArrayIndex] = range(start, end).map(i => ArrayIndex(i))

  implicit def builder[T: ClassTag](array: Array[T]): ImmutableArray[T] = ImmutableArray(array)


  def fromCsv[T: ClassTag](fileName: String,
                           separator: String = ",", skipHeaderLines: Int = 1)
                          (valueFromRow: ImmutableArray[String] => T): ImmutableArray[T] = {
    val bufferedSource = Source.fromFile(fileName)

    val rows = ImmutableArray(bufferedSource.getLines.map(line => {
      ImmutableArray(line.split(separator, -1))
    }).drop(skipHeaderLines))

    rows.map(valueFromRow)
  }

}


