package com.lkuligin.cats.Intro

import org.scalatest.{MustMatchers, WordSpec}
import cats._
import cats.implicits.eq
import CatEquality._

class EqualityTest extends WordSpec with MustMatchers {

  val testCat1 = Cat("Barsik", 5, "white")
  val testCat2 = Cat("Barsik", 5, "white")
  val testCat3 = Cat("Murzik", 5, "white")

  "equality test" in {
    catEquality.eqv(testCat1, testCat2) mustBe true
    (testCat1 === testCat2) mustBe true
    (testCat1 === testCat3) mustBe false
    (testCat1 !== testCat2) mustBe false
    (testCat1 !== testCat3) mustBe true
  }

}
