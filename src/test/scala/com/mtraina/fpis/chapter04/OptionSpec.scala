package com.mtraina.fpis.chapter04

import org.scalatest.{Matchers, FlatSpec}

class OptionSpec extends FlatSpec with Matchers {

  it should "map" in {
    val o = new Some[Int](5)
    o.map(x => x * 2) shouldBe None
  }
}
