package com.mtraina.fpis.chapter04

import org.scalatest.{Matchers, FlatSpec}

class OptionSpec extends FlatSpec with Matchers {

  "An option" should "map to None when is None" in {
    None map((x: Int) => x + 2) shouldBe None
  }

  it should "apply the function and return a Some of the result" in {
    Some(5) map(x => x * 2) shouldBe Some(10)
  }
}
