package com.lkuligin.cats.monoids

import org.scalatest.{MustMatchers, WordSpec}
import BooleanMonoids._

class BooleanMonoidsTest extends WordSpec with MustMatchers {

  "andMonoid" in {
    andMonoid.combine(false, true) mustBe false
    andMonoid.combine(false, false) mustBe false
    andMonoid.combine(true, true) mustBe true
    andMonoid.combine(false, andMonoid.empty) mustBe false
    andMonoid.combine(true, andMonoid.empty) mustBe true
  }

  "orMonoid" in {
    orMonoid.combine(false, true) mustBe true
    orMonoid.combine(false, false) mustBe false
    orMonoid.combine(true, true) mustBe true
    orMonoid.combine(false, orMonoid.empty) mustBe false
    orMonoid.combine(true, orMonoid.empty) mustBe true
  }

  "xorMonoid" in {
    xorMonoid.combine(false, true) mustBe true
    xorMonoid.combine(false, false) mustBe false
    xorMonoid.combine(true, true) mustBe false
    xorMonoid.combine(false, xorMonoid.empty) mustBe false
    xorMonoid.combine(true, xorMonoid.empty) mustBe true
  }

  "xnorMonoid" in {
    xnorMonoid.combine(false, true) mustBe false
    xnorMonoid.combine(false, false) mustBe true
    xnorMonoid.combine(true, true) mustBe true
    xnorMonoid.combine(false, xnorMonoid.empty) mustBe false
    xnorMonoid.combine(true, xnorMonoid.empty) mustBe true
  }
}
