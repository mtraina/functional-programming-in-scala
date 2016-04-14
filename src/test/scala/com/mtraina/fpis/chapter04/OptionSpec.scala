package com.mtraina.fpis.chapter04

import org.scalatest.{Matchers, FlatSpec}

import scala.util.Try

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

  it should "return None when filtering None" in {
    None.filter((x: Int) => x % 2 == 0) shouldBe None
  }

  it should "return None when is Some but the filter is not satisfied" in {
    Some(1).filter((x: Int) => x % 2 == 0) shouldBe None
  }

  it should "return itself when is a Some and it satisfies the filter" in {
    Some(4).filter((x: Int) => x % 2 == 0) shouldBe Some(4)
  }

  it should "calculate the variance of a valid sequence" in {
    Option.variance(List(1.0, 3.0, 5.0)) shouldBe Some(8/3d)
  }

  it should "return None when combining Options where at least one is None" in {
    Option.map2(None, None)((a: Int, b: Int) => a + b) shouldBe None
    Option.map2(None, Some(1))((a: Int, b: Int) => a + b) shouldBe None
    Option.map2(Some(1), None)((a: Int, b: Int) => a + b) shouldBe None

    // same using map2 with for-comprehension
    Option.map2_1(None, None)((a: Int, b: Int) => a + b) shouldBe None
    Option.map2_1(None, Some(1))((a: Int, b: Int) => a + b) shouldBe None
    Option.map2_1(Some(1), None)((a: Int, b: Int) => a + b) shouldBe None
  }

  it should "return a Some with the result of the applied function when both the Options are Some" in {
    Option.map2(Some(1), Some(2))((a: Int, b: Int) => a + b) shouldBe Some(3)

    // same using map2 with for-comprehension
    Option.map2_1(Some(1), Some(2))((a: Int, b: Int) => a + b) shouldBe Some(3)
  }

  it should "return None if combining a list of options it encounters a None" in {
    Option.sequence(List(Some(1), None)) shouldBe None
  }

  it should "return an Option containing a list of elements when combining a list of Some" in {
    Option.sequence(List(Some(1), Some(2), Some(3))) shouldBe Some(List(1,2,3))
  }

  it should "traverse the list and return None because there are Nones" in {
    Option.traverse(List(Some("a"),None,Some("c")))(i => i.map(ii => ii.toUpperCase)) shouldBe None
  }

  it should "traverse the list and compose an optional list" in {
    Option.traverse(List(Some("a"),Some("b"),Some("c")))(i => i.map(ii => ii.toUpperCase)) shouldBe Some(List("A","B","C"))
  }

  it should "traverse the list of non optionals and compose an optional list" in {
    Option.traverse(List("1","2","3"))(i => Some(Try(i.toInt).get)) shouldBe Some(List(1,2,3))
  }
}
