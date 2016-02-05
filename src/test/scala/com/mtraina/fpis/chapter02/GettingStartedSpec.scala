package com.mtraina.fpis.chapter02

import org.scalatest.{Matchers, FlatSpec}

class GettingStartedSpec extends FlatSpec with Matchers {

  "A Fibonacci calculation" should "return" in {
    GettingStarted.fib(0) should be (0)
    GettingStarted.fib(1) should be (1)
    GettingStarted.fib(2) should be (1)
    GettingStarted.fib(3) should be (2)
    GettingStarted.fib(4) should be (3)
    GettingStarted.fib(5) should be (5)
    GettingStarted.fib(10) should be (55)
  }
}
