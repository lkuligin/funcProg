package com.lkuligin.cats.monoids

import org.scalatest.{MustMatchers, WordSpec}
import SuperAdder._
import cats.Monoid

class SuperAdderTest extends WordSpec with MustMatchers {

  "adder should work for int" in {
    add(List(1,2,3)) mustBe 6
    add(List()) mustBe 0
  }

  "adder should work for options" in {
    import cats.instances.int._
    import cats.instances.option._
    add(List(Some(1), None, Some(2),Some(3))) mustBe Some(6)
    import cats.syntax.option._
    add(List(1.some, 2.some, 3.some)) mustBe Some(6)
  }

  "adder should work for Order" in {
    add(List(Order(1,2), Order(5,6))) mustBe Order(6,8)
  }
}
