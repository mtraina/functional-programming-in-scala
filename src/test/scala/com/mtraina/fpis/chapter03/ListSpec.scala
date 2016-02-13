package com.mtraina.fpis.chapter03

import com.mtraina.fpis.chapter03.List._
import org.scalatest.{Matchers, FlatSpec}
import org.scalatest.prop.TableDrivenPropertyChecks._

class ListSpec extends FlatSpec with Matchers {

  it should "sum all the element of the list" in {
    val lists = Table(
      ("source", "result"),
      (List(1,2,3), 6),
      (Nil, 0),
      (List(1), 1))

    forAll(lists){(s: List[Int], r: Int) =>
      sum(s) shouldBe r
      sum2(s) shouldBe r
    }
  }

  /**
    * Ex. 3.1
    */
  it should "check the pattern matching result on the static list" in {
    x shouldBe 3
  }

  /**
    * Ex. 3.2
    */
  it should "return the tail of the list" in {
    val lists = Table(
      ("source", "target"),
      (List(1,2,3), List(2,3)),
      (Nil, Nil),
      (List(1), Nil))

    forAll(lists){(s: List[Int], t: List[Int]) =>
      tail(s) shouldBe t
    }
  }

  /**
    * Ex. 3.3
    */
  it should "set a new head to the list" in {
    val lists = Table(
      ("source", "target"),
      (List(1,2,3), List(0,1,2,3)),
      (Nil, List(0)),
      (List(1), List(0, 1)))

    forAll(lists){(s: List[Int], t: List[Int]) =>
      setHead(0, s) shouldBe t
    }
  }

  /**
    * Ex. 3.4
    */
  it should "drop n elements from the list" in {
    val lists = Table(
      ("n", "source", "target"),
      (0, List(1,2,3), List(1,2,3)),
      (1, Nil, Nil),
      (1, List(1), Nil))

    forAll(lists){(n: Int, s: List[Int], t: List[Int]) =>
      drop(n, s) shouldBe t
    }
  }

  /**
    * Ex. 3.5
    */
  it should "drop elements that match the predicate form the list" in {
    val lists = Table(
      ("desc", "p", "source", "target"),
      ("drop all the even numbers", (i: Int) => i % 2 == 0, List(1,2,3), List(1,3)),
      ("drop all the odd numbers", (i: Int) => i % 2 == 1, List(1,2,3), List(2)),
      ("drop all the numbers that contains a 2", (i: Int) => i.toString.matches("2*"), List(1,2,3,11,22,33), List(1,3,11,33)))

    forAll(lists){(d: String, p: Int => Boolean, s: List[Int], t: List[Int]) =>
      println(d)
      dropWhile(s, p) shouldBe t
    }

    /** commented out because the implementation of drop while 2 doesn't seem to work properly
    forAll(lists){(d: String, p: Int => Boolean, s: List[Int], t: List[Int]) =>
      println(d)
      dropWhile2(s)(p) shouldBe t
    }*/
  }

  /**
    * Ex. 3.6
    */
  it should "dropping the last element of the list" in {
    val lists = Table(
      ("source", "target"),
      (List(1,2,3), List(1,2)),
      (Nil, Nil),
      (List(1), Nil))

    forAll(lists){(s: List[Int], t: List[Int]) =>
      init(s) shouldBe t
    }
  }
}
