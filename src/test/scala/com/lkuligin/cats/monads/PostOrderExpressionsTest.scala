package com.lkuligin.cats.monads

import org.scalatest.{MustMatchers, WordSpec}
import PostOrderExpressions._

class PostOrderExpressionsTest extends WordSpec with MustMatchers {
  "post order" in {
    evalOne("42").runA(Nil).value mustBe 42
    val input = evalAll(List("1", "2", "+", "3", "*"))
    input.runA(Nil).value mustBe 9
  }

}
