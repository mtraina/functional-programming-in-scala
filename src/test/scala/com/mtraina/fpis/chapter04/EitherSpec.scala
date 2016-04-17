package com.mtraina.fpis.chapter04

import org.scalatest.{FlatSpec, Matchers}

class EitherSpec extends FlatSpec with Matchers {

  "An Either" should "map to Left when it's a Left" in {
    Left("error") map((s: String) => s.toInt) shouldBe Left("error")
  }

  it should "apply the function and map to Right when it's a Right" in {
    Right("1") map((s: String) => s.toInt) shouldBe Right(1)
  }

  it should "return the Left as is when calling flatMap on Left" in {
    Left("fatal") flatMap((s: String) => Right(s.toUpperCase)) shouldBe Left("fatal")
  }

  it should "return a Right with the applied function when calling flatMap on it" in {
    Right("correct") flatMap((s: String) => Right(s.toUpperCase)) shouldBe Right("CORRECT")
  }

  it should "return the default value when calling orElse on a Left" in {
    Left("fail") orElse Right(1) shouldBe Right(1)
  }

  it should "return itself when calling orElse on a Right" in {
    Right(9) orElse Right(1) shouldBe Right(9)
  }

  it should "return Left when combining Either where at least one is a Left" in {
    val f = (a: String, b: String) => a + b
    Left("error").map2(Left("wrong"))(f) shouldBe Left("error")
    Left("error").map2(Right("correct"))(f) shouldBe Left("error")
    Right("correct").map2(Left("wrong"))(f) shouldBe Left("wrong")
  }

  it should "return a Right when combining two Right" in {
    Right(2).map2(Right(3))((a: Int, b: Int) => math.pow(a, b)) shouldBe Right(8)
  }

  it should "traverse the list and return a Left because there are Left" in {
    Either.traverse(List(Left("caution"), Right("true")))(i => i.map(ii => ii.toUpperCase)) shouldBe Left("caution")
  }

  it should "traverse the list and compose a list in a Right" in {
    Either.traverse(List(Right("a"), Right("b"), Right("c")))(i => i.map(ii => ii.toUpperCase)) shouldBe Right(List("A","B","C"))
  }

  it should "return a Left when combining a list of Either where at least one is a Left" in {
    Either.sequence(List(Left("warning"), Right("success"))) shouldBe Left("warning")
  }

  it should "return a Right containing the list of values when we pass a list of Right " in {
    Either.sequence(List(Right("perfect"), Right("success"))) shouldBe Right(List("perfect", "success"))
  }
}