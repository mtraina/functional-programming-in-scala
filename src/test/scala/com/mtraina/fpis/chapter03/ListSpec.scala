package com.mtraina.fpis.chapter03

import com.mtraina.fpis.chapter03.List._
import org.scalatest.{Matchers, FlatSpec}

class ListSpec extends FlatSpec with Matchers {

  it should "check the pattern matching result on the static list" in {
    x shouldBe 3
  }

  it should "return the tail of the list" in {
    tail(List(1,2,3)) shouldBe List(2,3)
  }
}
