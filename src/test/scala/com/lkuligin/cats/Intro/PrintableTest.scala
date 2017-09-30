package com.lkuligin.cats.Intro

import PrintableInstances._
import PrintableSyntax._
import org.scalatest.WordSpec
import cats.Show
import cats.instances.int._
import cats.instances.string._
import cats.syntax.show._
import CatEquality.Cat

class PrintableTest extends WordSpec {

  implicit val catPrintable = new Printable[Cat]{
    def format(cat: Cat) = {
      val name = Printable.format(cat.name)
      val age = Printable.format(cat.age)
      val color = Printable.format(cat.color)
      s"$name is a $age year-old and $color cat."
    }
  }

  implicit val catShow = Show.show[Cat] { cat =>
    val name = cat.name.show
    val age = cat.age.show
    val color = cat.color.show
    s"$name is a $age year-old $color cat."
  }

  "naive and better implementation" in {
    val testCat = Cat("Barsik", 5, "white")
    Printable.print(testCat)
    testCat.print
  }

  "cats implementation" in {
    val c: String = Cat("Barsik", 5, "white").show
    println(c)
  }

}
