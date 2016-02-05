package com.mtraina.fpis.chapter02

import com.mtraina.fpis.chapter02.GettingStarted._
import org.scalatest.{Matchers, FlatSpec}

class GettingStartedSpec extends FlatSpec with Matchers {

  "A factorial calculation" should "return" in {
    factorial(0) should be (1)
    factorial(3) should be (6)
    factorial(5) should be (120)
  }

  "A Fibonacci calculation" should "return" in {
    fib(0) should be (0)
    fib(1) should be (1)
    fib(2) should be (1)
    fib(3) should be (2)
    fib(4) should be (3)
    fib(5) should be (5)
    fib(10) should be (55)
  }
}
