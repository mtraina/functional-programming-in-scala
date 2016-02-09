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

  "It" should "return the first element of the collection using a polymorphic approach" in {
    findFirst(Array(0, 1, 2, 3), (n: Int) => n == 1) should be (1)
    findFirst(Array(0.1f, 1.2f, 2.3f, 3.4f), (f: Float) => f == 3.4f) should be (3)
    findFirst(Array(0.1, 1.2, 2.3, 3.4), (d: Double) => d == 3.4) should be (3)
  }

  "It" should "check if a polymorphic array is sorted according to a given comparison function" in {
    isSorted(Array.empty[String], (s: String, t: String) => s < t) shouldBe true
    isSorted(Array("alpha", "beta", "gamma"), (s: String, t: String) => s < t) shouldBe true
  }

  "It" should "apply a partial function" in {
    val partialSum = partial1(1, (a: Int, b: Int) => a + b)
    partialSum(0) shouldBe 1
    partialSum(2) shouldBe 3
    partialSum(-1) shouldBe 0
  }

  "It" should "curry the function" in {
    val curriedMul = curry((a: Int, b: Int) => a * b)
    curriedMul(2)(3) shouldBe 6
    curriedMul(1)(0) shouldBe 0
    curriedMul(-1)(-2) shouldBe 2
  }

  "It" should "uncurry the function" in {
    val curriedSub = curry((a: Int, b: Int) => a - b)
    val uncurriedSub = uncurry(curriedSub)
    uncurriedSub(2, 1) shouldBe 1
    uncurriedSub(-2, 1) shouldBe -3
    uncurriedSub(3, 0) shouldBe 3
  }

  "It" should "compose two functions" in {
    val comp = compose((b: Int) => b + 1, (a: Int) => a + 2)
    comp(2) shouldBe 5
    comp(0) shouldBe 3
    comp(-5) shouldBe -2
  }
}
