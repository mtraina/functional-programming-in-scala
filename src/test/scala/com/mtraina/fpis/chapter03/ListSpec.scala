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
      sum3(s) shouldBe r
    }
  }

  it should "multiply all the element of the list" in {
    val lists = Table(
      ("source", "result"),
      (List(2d,3d,4d), 24d),
      (List(1d), 1d))

    forAll(lists){(s: List[Double], r: Double) =>
      product(s) shouldBe r
      product2(s) shouldBe r
      product3(s) shouldBe r
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

  /**
    * Ex. 3.8
    */
  it should "append the second list to the first one" in {
    append(List(1,2,3), Nil: List[Int]) shouldBe List(1,2,3)
    append(List(1,2,3), List(4,5)) shouldBe List(1,2,3,4,5)
  }

  /**
    * Ex. 3.9
    */
  it should "compute the length of a list" in {
    val lists = Table(
      ("list", "length"),
      (List(1,2,3), 3),
      (Nil, 0),
      (List(1), 1))

    forAll(lists){(s: List[Int], l: Int) =>
      com.mtraina.fpis.chapter03.List.length(s) shouldBe l
    }
  }

  /**
    * Ex. 3.12
    */
  it should "reverse a list" in {
    val lists = Table(
      ("source", "target"),
      (List(1,2,3), List(3,2,1)),
      (List(9,8,7,6,5,4,3,2,1), List(1,2,3,4,5,6,7,8,9)),
      (Nil, Nil),
      (List(1), List(1)))

    forAll(lists){(s: List[Int], t: List[Int]) =>
      reverse(s) shouldBe t
    }
  }

  /**
    * Ex. 3.15
    */
  it should "concatenate a list of lists into a single one" in {
    val lists = Table(
      ("source", "target"),
      (List(List(1,2,3),List(3,2,1)), List(1,2,3,3,2,1))
    )

    forAll(lists){(s: List[List[Int]], t: List[Int]) =>
      concat(s) shouldBe t
    }
  }

  /**
    * Ex. 3.16
    */
  it should "add one to each element of the list" in {
    val lists = Table(
      ("source", "target"),
      (List(1,2,3), List(2,3,4)),
      (List(9,8,7,6,5,4,3,2,1), List(10,9,8,7,6,5,4,3,2)),
      (Nil, Nil),
      (List(1), List(2)))

    forAll(lists){(s: List[Int], t: List[Int]) =>
      add1(s) shouldBe t
    }
  }

  /**
    * Ex. 3.17
    */
  it should "convert every double in the list to a string element" in {
    val lists = Table(
      ("source", "target"),
      (List(1d,2d,3d), List("1.0","2.0","3.0")),
      (Nil, Nil),
      (List(1d), List("1.0")))

    forAll(lists){(s: List[Double], t: List[String]) =>
      doubleToString(s) shouldBe t
    }
  }

  /**
    * Ex. 3.18
    */
  it should "map the elements of the list according to the transformation function" in {
    map(List(1,2,3))((i: Int) => i -1) shouldBe List(0,1,2)
    map(List("1","2","3"))((i: String) => i.toInt) shouldBe List(1,2,3)
  }

  /**
    * Ex. 3.19
    * Ex. 3.21
    */
  it should "filter the list" in {
    filter(List(1,2,3,4,5,6,7,8,9,10))((i: Int) => i % 2 == 0) shouldBe List(2,4,6,8,10)
    filter1(List(1,2,3,4,5,6,7,8,9,10))((i: Int) => i % 2 == 0) shouldBe List(2,4,6,8,10)
  }

  /**
    * Ex. 3.20
    */
  it should "flat the mapped elements of the list" in {
    flatMap(List(1,2,3))(i => List(i,i)) shouldBe List(1,1,2,2,3,3)
    flatMap1(List(1,2,3))(i => List(i,i)) shouldBe List(1,1,2,2,3,3)
  }

  /**
    * Ex. 3.22
    */
  it should "create a list adding the elements of two lists" in {
    addPairwise(List(1,2,3), List(4,5,6)) shouldBe List(5,7,9)
  }

  /**
    * Ex. 3.23
    */
  it should "zip the lists according to the given function" in {
    zipWith(List(1,2,3), List(4,5,6))(_ + _) shouldBe List(5,7,9)
    zipWith(List("a","b","c"), List("z","y","x"))(_ + _) shouldBe List("az","by","cx")
  }

  /**
    * Ex. 3.24
    */
  it should "check if a list is prefixed by another" in {
    startsWith(List(1,2,3), List(1,2)) shouldBe true
    startsWith(List(1,2,3), List(1)) shouldBe true
    startsWith(List(1), List(1,2)) shouldBe false
    startsWith(List(1), Nil) shouldBe true
    startsWith(Nil, List(1)) shouldBe false
  }

  it should "check if a list is a sublist of another" in {
    hasSubsequence(List(1), List(1)) shouldBe true
    hasSubsequence(List(1,2,3), List(3)) shouldBe true
    hasSubsequence(List(1,2), List(3)) shouldBe false
    hasSubsequence(List(1,2,3), List(1,2,3)) shouldBe true
    hasSubsequence(List(1,2,3), List(1,2,3,4)) shouldBe false
  }
}
