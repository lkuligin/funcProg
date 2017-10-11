package com.lkuligin.foldables

import org.scalatest.{MustMatchers, WordSpec}
import com.lkuligin.cats.foldables.FoldableOps._

class FoldableOpsTest extends WordSpec with MustMatchers {
  "map" in {
    mapF(List(1,2,3))(x => x*2) mustBe List(2,4,6)
  }

  "flatMap" in {
    flatMapF(List(1,2,3))(x => List(x, x+1)) mustBe List(1,2,2,3,3,4)
  }

  "filter" in {
    filterF(List(1,2,3))(x => x % 2 == 1) mustBe List(1,3)
  }

  "reverse" in {
    reverseF(List(1,2,3)) mustBe List(3,2,1)
  }

}
