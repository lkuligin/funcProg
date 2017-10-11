package com.lkuligin.cats.monoids

import org.scalatest.{MustMatchers, WordSpec}
import SetMonoids._

class SetMonoidsTest extends WordSpec with MustMatchers {

  "intersection set semigroup" in {
    val intMonoid = intersectSemiGroup[Int]
    intMonoid.combine(Set(1,2,3), Set(2,3,4)) mustBe Set(2,3)
    intMonoid.combine(Set(1,2,3), Set(4,5)) mustBe Set[Int]()
    val stringMonoid = intersectSemiGroup[String]
    stringMonoid.combine(Set("a", "ab", "b"), Set("b", "ab", "bc")) mustBe Set("ab", "b")
  }

  "union set monoid" in {
    val intMonoid = unionMonoid[Int]
    intMonoid.combine(Set(1,2,3), Set(2,3,4)) mustBe Set(1,2,3,4)
    intMonoid.combine(Set(1,2,3), intMonoid.empty) mustBe Set(1,2,3)
    val stringMonoid = unionMonoid[String]
    stringMonoid.combine(Set("a", "ab", "b"), Set("b", "ab", "bc")) mustBe Set("a", "bc", "ab", "b")
    stringMonoid.combine(Set("a", "ab", "b"), stringMonoid.empty) mustBe Set("a", "ab", "b")
  }
}
