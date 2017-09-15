package fpinscala.datastructures

import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by P3trur0 (https://flatmap.it) on 15/09/17.
  */
class TreeSpec extends FlatSpec with Matchers {

  "Exercise 3.25" should "evaluate the size of a tree" in {
    Tree.size(Leaf(3)) shouldBe 1
    Tree.size(Branch(Leaf(3), Branch(Leaf(4), Leaf(5)))) shouldBe 5
  }

  "Exercise 3.26" should "evaluate the maximum value of a tree" in {
    Tree.maximum(Leaf(3)) shouldBe 3
    Tree.maximum(Branch(Leaf(3), Branch(Leaf(44), Leaf(5)))) shouldBe 44
  }

  "Exercise 3.27" should "evaluate the depth of a tree" in {
    Tree.depth(Leaf(3)) shouldBe 0
    Tree.depth(Branch(Leaf(3), Branch(Leaf(44), Leaf(5)))) shouldBe 1
  }

  "Exercise 3.28" should "perform a map operation on the tree" in {
    Tree.map(Leaf(3))(x => x.toString) shouldBe Leaf("3")
    Tree.map(Branch(Leaf(3), Branch(Leaf(44), Leaf(5))))(x => x*2) shouldBe Branch(Leaf(6), Branch(Leaf(88), Leaf(10)))
  }

  "Exercise 3.29" should "perform a generalization of the fold" in {
    Tree.sizeWithFold(Leaf(3)) shouldBe 1
    Tree.sizeWithFold(Branch(Leaf(3), Branch(Leaf(4), Leaf(5)))) shouldBe 5

    Tree.depthWithFold(Leaf(3)) shouldBe 0
    Tree.depthWithFold(Branch(Leaf(3), Branch(Leaf(44), Leaf(5)))) shouldBe 1

    Tree.maximumWithFold(Leaf(3)) shouldBe 3
    Tree.maximumWithFold(Branch(Leaf(3), Branch(Leaf(44), Leaf(5)))) shouldBe 44

    Tree.mapWithFold(Leaf(3))(x => x.toString) shouldBe Leaf("3")
    Tree.mapWithFold(Branch(Leaf(3), Branch(Leaf(44), Leaf(5))))(x => x*2) shouldBe Branch(Leaf(6), Branch(Leaf(88), Leaf(10)))
  }



}
