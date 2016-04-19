package com.mtraina.fpis.chapter05

import org.scalatest.{Matchers, FlatSpec}

class StreamSpec extends FlatSpec with Matchers {

  "A Stream" should "return an empty list when converting an empty" in {
    Empty.toList shouldBe Nil
  }
}
