package com.mtraina.fpis.chapter02

import org.scalatest.{Matchers, FlatSpec}

class Ex2_1Spec extends FlatSpec with Matchers {

  "A Fibonacci calculation" should "return" in {
    val ex2_1 = new Ex2_1
    ex2_1.fib(1) should be (1)
  }
}
