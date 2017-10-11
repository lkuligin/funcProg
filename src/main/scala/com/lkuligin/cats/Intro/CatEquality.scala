package com.lkuligin.cats.Intro

import cats.Eq
import cats.syntax.eq._

object CatEquality {
  final case class Cat(name: String, age: Int, color: String)

  implicit val catEquality = Eq.instance[Cat] {
    (cat1, cat2) =>
      import cats.instances.int._
      import cats.instances.string._

      (cat1.name === cat2.name) && (cat1.age === cat2.age) && (cat1.color === cat2.color)
  }

  def demo() = {
    val testCat1 = Cat("Barsik", 5, "white")
    val testCat2 = Cat("Barsik", 5, "white")
    val testCat3 = Cat("Murzik", 5, "white")

    println(testCat1 === testCat2)
    println(testCat1 =!= testCat3)

    import cats.instances.option._

    println(Option(testCat1) === Option(testCat2))
    println(Option(testCat1) =!= Option(testCat3))
  }
}
