package com.mtraina.fpis.chapter02

import com.mtraina.fpis.chapter02.GettingStarted._
import org.scalatest.prop.TableDrivenPropertyChecks._
import org.scalatest.{FlatSpec, Matchers}

class GettingStartedSpec extends FlatSpec with Matchers {

  "A factorial calculation" should "return" in {
    val factNumbers = Table(
      ("number", "result"),
      (0, 1),
      (3, 6),
      (5, 120),
      (10, 3628800)
    )

    forAll(factNumbers){(n: Int, r: Int) =>
      factorial(n) should be (r)
    }
  }

  "A Fibonacci calculation" should "return" in {
    val fibNumbers = Table(
      ("number", "result"),
      (0, 0),
      (1, 1),
      (2, 1),
      (3, 2),
      (4, 3),
      (5, 5),
      (10, 55)
    )

    forAll(fibNumbers){(n: Int, r: Int) =>
      fib(n) should be (r)
    }
  }
}
