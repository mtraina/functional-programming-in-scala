package com.mtraina.fpis.chapter07

import org.scalatest.{FlatSpec, Matchers}

class ParSpec extends FlatSpec with Matchers {
  /**
    * Ex. 7.1
    */
  "A sum using divide and conquer algorithm" should "calculate the sum in parallel" in {
    Examples.sum(IndexedSeq(1,2,3)) shouldBe 6
  }
}
