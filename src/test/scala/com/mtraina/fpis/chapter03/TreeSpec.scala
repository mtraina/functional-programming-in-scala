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

  /**
    * Ex. 3.27
    */
  it should "calculate the max depth of the tree" in {
    depth(Leaf(1)) shouldBe 0
    depth(Branch(Leaf(1), Branch(Leaf(1), Branch(Leaf(1), Branch(Leaf(10), Leaf(1)))))) shouldBe 4
    depth(Branch(
      Branch(Leaf(1),
        Branch(Leaf(1),
          Branch(Leaf(10),
            Branch(Leaf(1),
              Branch(Leaf(1), Leaf(1))
            )
          )
        )
      ), Leaf(2))) shouldBe 6
  }

  /**
    * Ex. 3.28
    */
  it should "map each element of the tree" in {
    map(Leaf(1))(x => x * 2) shouldBe Leaf(2)
    map(Branch(Leaf(1), Branch(Leaf(1), Branch(Leaf(1), Branch(Leaf(10), Leaf(1))))))(x => x + 3) shouldBe
      Branch(Leaf(4), Branch(Leaf(4), Branch(Leaf(4), Branch(Leaf(13), Leaf(4)))))
  }
}
