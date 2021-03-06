package com.mtraina.fpis.chapter06

import com.mtraina.fpis.chapter06.RNG._
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

    /**
      * Ex. 6.5
      */
    val d1 = RNG.doubleMap(SimpleRNG(1))
    d1._1 >= 0d && d1._1 < 1 shouldBe true
  }

  /**
    * Ex. 6.3
    */
  it should "generate a tuple" in {
    // TODO is not checking the type of the tuple (type erasure?)
    RNG.intDouble(SimpleRNG(2)) shouldBe a [((Int, Double), RNG)]
    RNG.doubleInt(SimpleRNG(2)) shouldBe a [((Double, Int), RNG)]
    RNG.double3(SimpleRNG(2)) shouldBe a [((Double, Double, Double), RNG)]
  }

  /**
    * Ex. 6.4
    */
  it should "generate a list of ints" in {
    RNG.ints(2)(SimpleRNG(3))._1.length shouldBe 2
    RNG.ints(12)(SimpleRNG(100))._1.length shouldBe 12
  }

  /**
    * Ex. 6.5
    */
  it should "map an int wrapped in a rand to a string wrapped in a rand" in {
    val rng: RNG = SimpleRNG(1)
    val f: Rand[Int] = RNG.int
    val g: Int => String = n => "hello"
    val r: Rand[String] = map(f)(g)

    r(rng)._1 shouldEqual "hello"
  }

  /**
    * Ex. 6.8
    */
  it should "flat map an int wrapped in a rand to a string wrapped in a rand" in {
    val rng: SimpleRNG = SimpleRNG(1)
    val f: Rand[Int] = RNG.int
    val g: (Int) => Rand[String] = n => unit("hello")
    val r: Rand[String] = flatMap(f)(g)

    r(rng)._1 shouldEqual "hello"
  }
}
