package com.lkuligin.cats

import com.lkuligin.cats.functors.BinaryTree._
import org.scalatest.{MustMatchers, WordSpec}
import cats.syntax.functor._

class BinaryTreeTest extends  WordSpec with MustMatchers {
  "binary tree functor" in {
    branch(branch(leaf(1), leaf(2)), leaf(3)).map(_ + 5) mustBe Branch(Branch(Leaf(6), Leaf(7)), Leaf(8))
  }
}
