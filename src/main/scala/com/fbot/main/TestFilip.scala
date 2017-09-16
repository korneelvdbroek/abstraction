package com.fbot.main

import com.fbot.algos.mutualinformation.{DelayIndependentMutualInformation, MutualInformation}
import com.fbot.common.fastcollections.ImmutableArray
import com.fbot.common.hyperspace.Tuple

import scala.util.Random

/**
  * Created by filip on 16/09/2017.
  */
object TestFilip {

  def main(args: Array[String]): Unit = {

    val N = 1000
    val dim = 1
    val shift = 10

    val k = 100

    val dataX = ImmutableArray.fill[Tuple](N)(Tuple.fill(dim)(randomDouble))
    val dataShiftX = getShiftedVersion(dataX, shift)

    val dataY = ImmutableArray.fill[Tuple](N)(Tuple.fill(dim)(randomDouble))
    val dataShiftY = getShiftedVersion(dataY, shift)


    println(MutualInformation(dataX, dataX).MI(k))
    println(MutualInformation(dataX, dataShiftX).MI(k))
    println(DelayIndependentMutualInformation(dataX, dataShiftX, 20).MI(k))
  }


  def randomDouble = {
    Random.nextDouble() * 1000d
  }

  def getShiftedVersion(data: ImmutableArray[Tuple], shift: Int) : ImmutableArray[Tuple] = {
    val part1 = (0 to shift).map(_ => Tuple(randomDouble)).toList
    val part2 = data.repr.take(data.length - shift).toList

    val combined: List[Tuple] = part1 ::: part2
    ImmutableArray(combined.toArray)
  }



}
