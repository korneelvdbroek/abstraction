package com.fbot.common.linalg

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

}

object SpecRichDenseMatrix {

  val A = new DenseMatrix(2, 2, Array(1, 2, 3, 4))

  val B = new DenseMatrix(2, 2, Array(5, 6, 7, 8))

}

