package com.mtraina.fpis.chapter02

import org.scalatest.{Matchers, FlatSpec}

class Ex2_1Spec extends FlatSpec with Matchers {

  "A Fibonacci calculation" should "return" in {
    val ex2_1 = new Ex2_1
    ex2_1.fib(0) should be (0)
    ex2_1.fib(1) should be (1)
    ex2_1.fib(2) should be (1)
    ex2_1.fib(3) should be (2)
    ex2_1.fib(4) should be (3)
    ex2_1.fib(5) should be (5)
    ex2_1.fib(10) should be (55)
  }
}
