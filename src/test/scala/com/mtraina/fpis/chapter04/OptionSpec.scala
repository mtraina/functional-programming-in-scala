package com.mtraina.fpis.chapter04

import org.scalatest.{Matchers, FlatSpec}

class OptionSpec extends FlatSpec with Matchers {

  "An Option" should "map to None when is None" in {
    None map((x: Int) => x + 2) shouldBe None
  }

  it should "apply the function and return a Some of the result" in {
    Some(5) map(x => x * 2) shouldBe Some(10)
  }

  it should "flat map to None when is None" in {
    None.flatMap((x: Int) => Some(x + 10)) shouldBe None
  }

  it should "flat map to a Some of the applied function when it is a Some" in {
    Some(3).flatMap((x: Int) => Some(x + 10)) shouldBe Some(13)
  }

  it should "return the default value when is None" in {
    None.getOrElse(1) shouldBe 1
  }

  it should "get value of the Option when is a Some" in {
    Some(20).getOrElse(1) shouldBe 20
  }

  it should "return the second option when is None" in {
    None.orElse(Some(10)) shouldBe Some(10)
  }

  it should "return the first option when is Some" in {
    Some(5).orElse(Some(10)) shouldBe Some(5)
  }
}
