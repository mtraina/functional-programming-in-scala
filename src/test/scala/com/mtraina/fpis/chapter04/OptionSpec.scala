package com.mtraina.fpis.chapter04

import org.scalatest.{Matchers, FlatSpec}

class OptionSpec extends FlatSpec with Matchers {

  "An Option" should "map to None when is None" in {
    None map((x: Int) => x + 2) shouldBe None
  }

  it should "apply the function and return a Some of the result" in {
    Some(5) map(x => x * 2) shouldBe Some(10)
  }

  "An Option" should "flat map" in {

  }

  "An Option" should "return the default value when is None" in {
    None.getOrElse(1) shouldBe 1
  }

  it should "get value of the Option when is a Some" in {
    Some(20).getOrElse(1) shouldBe 20
  }
}
