package com.mtraina.fpis.chapter06

import com.mtraina.fpis.chapter06.RNG.Simple
import org.scalatest.{FlatSpec, Matchers}

class StateSpec extends FlatSpec with Matchers {

  "A RNG" should "generate a non negative integer" in {
    val rng = Simple(3)
    RNG.nonNegativeInt(rng) shouldBe rng.nextInt

    val rng2 = Simple(Int.MinValue)
    RNG.nonNegativeInt(rng2) shouldBe rng2.nextInt
  }
}
