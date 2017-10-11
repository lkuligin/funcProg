package com.lkuligin.cats.Intro

import com.lkuligin.cats.Intro.CatEquality._
import org.scalatest.{MustMatchers, WordSpec}

class EqualityTest extends WordSpec with MustMatchers {

  demo()

  val testCat1 = Cat("Barsik", 5, "white")
  val testCat2 = Cat("Barsik", 5, "white")
  val testCat3 = Cat("Murzik", 5, "white")

  "equality test" in {
    (testCat1 === testCat2) mustBe true //=== comes from scalactic and not from cats
    (testCat1 === testCat3) mustBe false
    (testCat1 !== testCat2) mustBe false
    (testCat1 !== testCat3) mustBe true
  }

}
