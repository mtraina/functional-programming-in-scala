package com.mtraina.fpis.chapter05

import org.scalatest.{Matchers, FlatSpec}
import com.mtraina.fpis.chapter05.Stream._

class StreamSpec extends FlatSpec with Matchers {

  /**
    * Ex. 5.1
    */
  "A Stream" should "return an empty list when converting an Empty" in {
    Empty.toList shouldBe Nil
  }

  it should "convert to a list when operating on a Cons" in {
    cons(1, Empty).toList shouldBe List(1)
    cons(1, cons(2, cons(3, Empty))).toList shouldBe List(1,2,3)
  }

  /**
    * Ex. 5.2
    */
  it should "return the first n elements of a Stream" in {
    cons(1, Empty).take(1).toList shouldBe List(1)
    cons(1, cons(2, cons(3, Empty))).take(2).toList shouldBe List(1,2)
  }

  it should "return the last n elements of a Stream" in {
    cons(1, Empty).drop(1).toList shouldBe empty
    cons(1, cons(2, cons(3, Empty))).drop(2).toList shouldBe List(3)
  }
}
