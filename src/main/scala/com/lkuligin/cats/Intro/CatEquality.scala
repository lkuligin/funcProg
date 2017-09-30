package com.lkuligin.cats.Intro

import cats.Eq
import cats.syntax.eq._

object CatEquality {
  final case class Cat(name: String, age: Int, color: String)

  implicit val catEquality = Eq.instance[Cat] {
    (cat1, cat2) =>
      import cats.instances.int
      import cats.instances.string

      (cat1.name == cat2.name) && (cat1.age == cat2.age) && (cat1.color == cat2.color)
  }
}
