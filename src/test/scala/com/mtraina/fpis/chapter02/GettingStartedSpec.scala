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

  "Format result" should "return a formatted string" in {
    formatResult("the factorial of %d is %d", 3, factorial) should be ("the factorial of 3 is 6")

    formatResult("Fibonacci of %d is %d", 5, fib) should be ("Fibonacci of 5 is 5")
  }

  "It" should "return the first element of the collection" in {
    findFirst(Array("a", "b", "c"), "a") should be (0)
    findFirst(Array("a", "b", "c"), "c") should be (2)
    findFirst(Array("a", "b", "c"), "d") should be (-1)
  }
}
