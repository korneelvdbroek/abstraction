package com.fbot.common.linalg

import breeze.linalg.max
import com.fbot.common.linalg.RichDenseMatrix._
import org.apache.spark.mllib.linalg.DenseMatrix
import org.scalatest.{FlatSpec, Matchers}

/**
  *
  */
class SpecRichDenseMatrix extends FlatSpec with Matchers {

  import SpecRichDenseMatrix._

  "diagMultiply" should "return the diagonal of the multiplication of matrices A and B" in {
    A.diagMultiply(B) shouldBe new DenseMatrix(1, 2, Array(23, 46))
  }

  it should "return the diagonal of the multiplication of matrices A.T and B" in {
    A.transpose.diagMultiply(B) shouldBe new DenseMatrix(1, 2, Array(17, 53))
  }

  it should "return the diagonal of the multiplication of matrices A and B.T" in {
    A.diagMultiply(B.transpose) shouldBe new DenseMatrix(1, 2, Array(26, 44))
  }

  it should "return the diagonal of the multiplication of matrices A.T and B.T" in {
    A.transpose.diagMultiply(B.transpose) shouldBe new DenseMatrix(1, 2, Array(19, 50))
  }

  "colSums" should "sums of the columns of A into a row vector" in {
    A.colSums shouldBe new DenseMatrix(1, 2, Array(3, 7))
  }

  it should "sums of the columns of A.T into a row vector" in {
    A.transpose.colSums shouldBe new DenseMatrix(1, 2, Array(4, 6))
  }

  "rowSums" should "sums of the columns of A into a row vector" in {
    A.rowSums shouldBe new DenseMatrix(2, 1, Array(4, 6))
  }

  it should "sums of the columns of A.T into a row vector" in {
    A.transpose.rowSums shouldBe new DenseMatrix(2, 1, Array(3, 7))
  }

  "mapWithIndex" should "map the elements of A" in {
    A.mapWithIndex((i, j, v) => v + i + 2*j) shouldBe new DenseMatrix(2, 2, Array(1+0+2*0, 2+1+2*0, 3+0+2*1, 4+1+2*1))
  }

  "fold" should "find the maximum element in A" in {
    A.fold(A(0, 0))(max(_, _)) shouldBe 4d
  }

}

object SpecRichDenseMatrix {

  val A = new DenseMatrix(2, 2, Array(1, 2, 3, 4))

  val B = new DenseMatrix(2, 2, Array(5, 6, 7, 8))

}

