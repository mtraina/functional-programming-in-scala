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

  /**
    * Ex. 5.3
    */
  it should "return all starting elements matching the predicate" in {
    cons(2, cons(4, cons(1, Empty))).takeWhile(x => x % 2 == 0).toList shouldBe List(2,4)
    cons(1, cons(2, cons(3, cons(4, cons(5, Empty))))).takeWhile(x => x < 3).toList shouldBe List(1, 2)
    cons(1, cons(2, cons(3, cons(4, cons(5, Empty))))).takeWhile(x => x > 3).toList shouldBe List()
  }

  it should "check if an element matching the predicate exists" in {
    cons(2, cons(4, cons(1, Empty))).exists(x => x % 2 == 0) shouldBe true
    cons(2, cons(4, cons(1, Empty))).exists(x => x == 0) shouldBe false
  }

  /**
    * Ex. 5.4
    */
  it should "check if all the elements match the predicate" in {
    cons(2, cons(4, cons(1, Empty))).forAll(x => x % 2 == 0) shouldBe false
    cons(2, cons(4, cons(8, Empty))).forAll(x => x % 2 == 0) shouldBe true
    cons(2, cons(4, cons(1, Empty))).forAll(x => x > 0) shouldBe true
  }
}
