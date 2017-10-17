package com.mtraina.fpis.chapter08

import org.scalatest.{FunSpec, GivenWhenThen, Matchers}

class PropertySpec extends FunSpec with Matchers with GivenWhenThen {
  it("should check if a list is valid") {
    PropertyTest.checkList
  }
}
