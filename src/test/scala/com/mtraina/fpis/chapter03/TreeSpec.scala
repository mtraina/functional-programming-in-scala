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

}
