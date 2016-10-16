package com.mtraina.fpis.chapter06

import com.mtraina.fpis.chapter06.RNG.SimpleRNG
import org.scalatest.{FlatSpec, Matchers}

class StateSpec extends FlatSpec with Matchers {

  /**
    * Ex. 6.1
    */
  "A RNG" should "generate a non negative integer" in {
    val rng = SimpleRNG(3)
    RNG.nonNegativeInt(rng) shouldBe rng.nextInt

    val rng2 = SimpleRNG(Int.MinValue).nextInt
    if(rng2._1 >= 0){
      RNG.nonNegativeInt(SimpleRNG(Int.MinValue)) shouldBe (rng2._1, rng2._2)
    } else {
      RNG.nonNegativeInt(SimpleRNG(Int.MinValue)) shouldBe (-rng2._1 - 1, rng2._2)
    }
  }

  /**
    * Ex. 6.2
    */
  it should "generate a double between 0 and 1 not included" in {
    val rng = SimpleRNG(1)
    val d = RNG.double(rng)
    d._1 >= 0d && d._1 < 1 shouldBe true
  }
}
