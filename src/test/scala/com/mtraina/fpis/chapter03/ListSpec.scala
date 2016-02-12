package com.mtraina.fpis.chapter03

import com.mtraina.fpis.chapter03.List._
import org.scalatest.{Matchers, FlatSpec}
import org.scalatest.prop.TableDrivenPropertyChecks._

class ListSpec extends FlatSpec with Matchers {

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
      ("p", "source", "target"),
      ((i: Int) => i % 2 == 0, List(1,2,3), List(1,3)))

    forAll(lists){(p: Int => Boolean, s: List[Int], t: List[Int]) =>
      dropWhile(p, s) shouldBe t
    }
  }
}
