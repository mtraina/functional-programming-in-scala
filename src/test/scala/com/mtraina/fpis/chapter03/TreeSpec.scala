package com.mtraina.fpis.chapter03

import com.mtraina.fpis.chapter03.Tree._
import org.scalatest.{Matchers, FlatSpec}

class TreeSpec extends FlatSpec with Matchers {
  /**
    * Ex. 3.25
    */
  it should "calculate the size of the tree" in {
    size(treeSize)(Leaf(2)) shouldBe 1
    size(treeSize)(Branch(Leaf(1), Leaf(2))) shouldBe 3
    size(treeSize)(Branch(Leaf(1), Branch(Leaf(1), Branch(Leaf(1), Branch(Leaf(1), Leaf(1)))))) shouldBe 9

    // first refactoring, using fold
    size(treeSize1)(Leaf(2)) shouldBe 1
    size(treeSize1)(Branch(Leaf(1), Leaf(2))) shouldBe 3
    size(treeSize1)(Branch(Leaf(1), Branch(Leaf(1), Branch(Leaf(1), Branch(Leaf(1), Leaf(1)))))) shouldBe 9

    // second refactoring, even less code in the definition of the accumulator function
    size(treeSize2)(Leaf(2)) shouldBe 1
    size(treeSize2)(Branch(Leaf(1), Leaf(2))) shouldBe 3
    size(treeSize2)(Branch(Leaf(1), Branch(Leaf(1), Branch(Leaf(1), Branch(Leaf(1), Leaf(1)))))) shouldBe 9

    def size[A](f: Tree[A] => Int)(t: Tree[A]): Int = f(t)
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

    maximum2(Leaf(1)) shouldBe 1
    maximum2(Branch(Leaf(1), Branch(Leaf(1), Branch(Leaf(1), Branch(Leaf(10), Leaf(1)))))) shouldBe 10
    maximum2(Branch(Leaf(20), Branch(Leaf(1), Branch(Leaf(1), Branch(Leaf(10), Leaf(1)))))) shouldBe 20

    maximum3(Leaf(1)) shouldBe 1
    maximum3(Branch(Leaf(1), Branch(Leaf(1), Branch(Leaf(1), Branch(Leaf(10), Leaf(1)))))) shouldBe 10
    maximum3(Branch(Leaf(20), Branch(Leaf(1), Branch(Leaf(1), Branch(Leaf(10), Leaf(1)))))) shouldBe 20
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

    depth1(Leaf(1)) shouldBe 0
    depth1(Branch(Leaf(1), Branch(Leaf(1), Branch(Leaf(1), Branch(Leaf(10), Leaf(1)))))) shouldBe 4
    depth1(Branch(
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

    map1(Leaf(1))(x => x * 2) shouldBe Leaf(2)
    map1(Branch(Leaf(1), Branch(Leaf(1), Branch(Leaf(1), Branch(Leaf(10), Leaf(1))))))(x => x + 3) shouldBe
      Branch(Leaf(4), Branch(Leaf(4), Branch(Leaf(4), Branch(Leaf(13), Leaf(4)))))

    map2(Leaf(1))(x => x * 2) shouldBe Leaf(2)
    map2(Branch(Leaf(1), Branch(Leaf(1), Branch(Leaf(1), Branch(Leaf(10), Leaf(1))))))(x => x + 3) shouldBe
      Branch(Leaf(4), Branch(Leaf(4), Branch(Leaf(4), Branch(Leaf(13), Leaf(4)))))
  }
}
