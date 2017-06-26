package com.mtraina.fpis.chapter07

import java.util.concurrent.Executors

import org.scalatest.{FunSpec, GivenWhenThen, Matchers}

class ParSpec extends FunSpec with Matchers with GivenWhenThen {

  /**
    * Ex. 7.1
    */
  it("should sum using divide and conquer algorithm should calculate the sum in parallel") {
    Examples.sum(IndexedSeq(1,2,3)) shouldBe 6
  }

  describe("A Par") {
    /**
      * Ex. 7.4
      */
    it("should transform a synchronous function to an async one") {
      Given("an executor service")
      val es = Executors.newSingleThreadExecutor()

      When("calling async function")
      val af = Par.asyncF((a: Int) => a.toString)

      Then("it should return a function that transforms a sync to an async function, where applying an argument " +
        "and then an executor service returns a future where we can force to get the value from")
      af.apply(1).apply(es).get shouldBe "1"
    }
  }
}
