package com.mtraina.fpis.chapter05

import org.scalatest.{Matchers, FlatSpec}
import com.mtraina.fpis.chapter05.Stream._

class StreamSpec extends FlatSpec with Matchers {

  "A Stream" should "return the optional head" in {
    Empty.headOption shouldBe None
    Stream(2, 4, 1).headOption shouldBe Some(2)

    /**
      * Ex. 5.6
      */
    Empty.headOption2 shouldBe None
    Stream(2, 4, 1).headOption2 shouldBe Some(2)
  }

  /**
    * Ex. 5.1
    */
  it should "return an empty list when converting an Empty" in {
    Empty.toList shouldBe Nil
  }

  it should "convert to a list when operating on a Cons" in {
    Stream(1).toList shouldBe List(1)
    Stream(1, 2, 3).toList shouldBe List(1,2,3)
  }

  /**
    * Ex. 5.2
    */
  it should "return the first n elements of a Stream" in {
    Stream(1).take(1).toList shouldBe List(1)
    Stream(1, 2, 3).take(2).toList shouldBe List(1,2)
  }

  it should "return the last n elements of a Stream" in {
    Stream(1).drop(1).toList shouldBe empty
    Stream(1, 2, 3).drop(2).toList shouldBe List(3)
  }

  /**
    * Ex. 5.3
    */
  it should "return all starting elements matching the predicate" in {
    Stream(2, 4, 1).takeWhile(x => x % 2 == 0).toList shouldBe List(2,4)
    Stream(1, 2, 3, 4, 5).takeWhile(x => x < 3).toList shouldBe List(1, 2)
    Stream(1, 2, 3, 4, 5).takeWhile(x => x > 3).toList shouldBe List()

    /**
      * Ex. 5.5
      */
    Stream(2, 4, 1).takeWhile2(x => x % 2 == 0).toList shouldBe List(2,4)
    Stream(1, 2, 3, 4, 5).takeWhile2(x => x < 3).toList shouldBe List(1, 2)
    Stream(1, 2, 3, 4, 5).takeWhile2(x => x > 3).toList shouldBe List()
  }

  it should "check if an element matching the predicate exists" in {
    Stream(2, 4, 1).exists(x => x % 2 == 0) shouldBe true
    Stream(2, 4, 1).exists(x => x == 0) shouldBe false
  }

  /**
    * Ex. 5.4
    */
  it should "check if all the elements match the predicate" in {
    Stream(2, 4, 1).forAll(x => x % 2 == 0) shouldBe false
    Stream(2, 4, 8).forAll(x => x % 2 == 0) shouldBe true
    Stream(2, 4, 1).forAll(x => x > 0) shouldBe true
  }

  /**
    * Ex. 5.7
    */
  it should "implement map, filter, append and flat map using fold right" in {
    // map
    Stream(1, 2, 3).map(x => x + 1).toList shouldBe List(2, 3, 4)

    // filter
    Stream(1, 2, 3).filter(x => x % 3 == 0).toList shouldBe List(3)

    // append
    Stream(1, 2).append(cons(3, Empty)).toList shouldBe List(1, 2, 3)

    // flat map
    Stream(3, 4).flatMap(x => cons(x * 2, Empty)).toList shouldBe List(6, 8)
  }

  /**
    * Ex. 5.8
    */
  it should "return an infinite sequence of constants" in {
    Stream.constant('a').take(5).toList shouldBe List('a', 'a', 'a', 'a', 'a')
  }

  /**
    * Ex. 5.9
    */
  it should "return an infinite and incremental sequence" in {
    Stream.from(5).take(3).toList shouldBe List(5, 6, 7)
    Stream.from(99).take(5).toList shouldBe List(99, 100, 101, 102, 103)
  }
}
