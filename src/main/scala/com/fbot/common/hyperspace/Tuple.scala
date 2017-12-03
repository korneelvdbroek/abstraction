package com.fbot.common.hyperspace

import com.fbot.common.fastcollections.{BuilderFromArray, FastTuple, ImmutableArray}
import com.fbot.common.fastcollections.math.FastTupleDoubleMath

import scala.collection.mutable
import scala.reflect.ClassTag

/**
  *
  */
case class Tuple(repr: mutable.WrappedArray[Double]) extends AnyVal with FastTuple[Double, Tuple] with FastTupleDoubleMath[Tuple] {

  def make(x: mutable.WrappedArray[Double]): Tuple = Tuple(x)

  def dim: Int = repr.length

  def slice(from: Int, until: Int): Tuple = Tuple(repr.slice(from, until))

  override def toString: String = {
    repr.mkString("(", ", ", ")")
  }

}

object Tuple {

  def apply(data: Double*): Tuple = new Tuple(data.toArray)

  def apply(data: TraversableOnce[Double]): Tuple = Tuple(data.toArray)

  def fill(dim: Int)(elem: ⇒ Double): Tuple = Tuple(Array.fill(dim)(elem))

  implicit def fromImmutableArray(array: ImmutableArray[Double]): Tuple = Tuple(array.repr)

  implicit def builderFromArray[T](implicit m: ClassTag[T]): BuilderFromArray[Double, Tuple] = {
    new BuilderFromArray[Double, Tuple] {
      def result(array: Array[Double]): Tuple = Tuple(array)
    }
  }

}


