package com.mtraina.fpis.chapter03

import com.mtraina.fpis.chapter03.Tree._
import org.scalatest.{Matchers, FlatSpec}

class TreeSpec extends FlatSpec with Matchers {
  /**
    * Ex. 3.25
    */
  it should "calculate the size of the tree" in {
    treeSize(Leaf(2)) shouldBe 1
    treeSize(Branch(Leaf(1), Leaf(2))) shouldBe 2
    treeSize(Branch(Leaf(1), Branch(Leaf(1), Branch(Leaf(1), Branch(Leaf(1), Leaf(1)))))) shouldBe 5
  }

  /**
    * Ex. 3.26
    */
  it should "calculate the maximum element of the tree" in {
    maximum(Leaf(1)) shouldBe 1
    maximum(Branch(Leaf(1), Branch(Leaf(1), Branch(Leaf(1), Branch(Leaf(10), Leaf(1)))))) shouldBe 10
    maximum(Branch(Leaf(20), Branch(Leaf(1), Branch(Leaf(1), Branch(Leaf(10), Leaf(1)))))) shouldBe 20

    maximum1(Leaf(1)) shouldBe 1
    maximum1(Branch(Leaf(1), Branch(Leaf(1), Branch(Leaf(1), Branch(Leaf(10), Leaf(1)))))) shouldBe 10
    maximum1(Branch(Leaf(20), Branch(Leaf(1), Branch(Leaf(1), Branch(Leaf(10), Leaf(1)))))) shouldBe 20
  }
}
