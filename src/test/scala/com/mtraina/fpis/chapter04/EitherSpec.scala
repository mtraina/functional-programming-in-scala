package com.mtraina.fpis.chapter04

import org.scalatest.{Matchers, FlatSpec}

class EitherSpec extends FlatSpec with Matchers {

  "An Either" should "map to Left when it's a Left" in {
    Left("error") map((s: String) => s.toInt) shouldBe Left("error")
  }

  it should "apply the function and map to Right when it's a Right" in {
    Right("1") map((s: String) => s.toInt) shouldBe Right(1)
  }
}
