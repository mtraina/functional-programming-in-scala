package com.mtraina.fpis.chapter03

import org.scalatest.{Matchers, FlatSpec}

class ListSpec extends FlatSpec with Matchers {

  "It" should "check the pattern matching result on the static list" in {
    List.x shouldBe 3
  }
}
