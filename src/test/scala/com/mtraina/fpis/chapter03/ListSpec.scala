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


}
