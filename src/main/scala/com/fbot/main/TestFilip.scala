package com.fbot.main

import com.fbot.algos.mutualinformation.{DelayIndependentMutualInformation, MutualInformation}
import com.fbot.common.fastcollections.{ImmutableArray, ImmutableTupleArray, Tuple}
import com.fbot.common.fastcollections._

import scala.util.Random

/**
  * Copyright (C) 2017-2018  filip
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
  */
object TestFilip {

  def main(args: Array[String]): Unit = {

    val N = 1000
    val dim = 1
    val shift = 10

    val k = 100

    val dataX = ImmutableTupleArray.fill(N)(Tuple.fill(dim)(randomDouble))
    val dataShiftX = getShiftedVersion(dataX, shift)

    val dataY = ImmutableTupleArray.fill(N)(Tuple.fill(dim)(randomDouble))
    val dataShiftY = getShiftedVersion(dataY, shift)


    println(MutualInformation(dataX, dataX).MI(k))
    println(MutualInformation(dataX, dataShiftX).MI(k))
    println(DelayIndependentMutualInformation(dataX, dataShiftX, 20).MI(k))
  }


  def randomDouble = {
    Random.nextDouble() * 1000d
  }

  def getShiftedVersion(data: ImmutableTupleArray, shift: Int) : ImmutableTupleArray = {
    val part1 = (0 to shift).map(_ => Tuple(randomDouble)).toList
    val part2 = data.take(data.length - shift).toArray.toList

    val combined: List[Tuple] = part1 ::: part2
    ImmutableTupleArray.fromTuples(ImmutableArray(combined.toArray))
  }



}
