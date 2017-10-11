package com.lkuligin.cats.functor

import com.lkuligin.cats.functors.Printable
import org.scalatest.{MustMatchers, WordSpec}
import com.lkuligin.cats.functors.Printable._
import cats.instances.string._

class PrintableTest extends WordSpec with MustMatchers {

  final case class Box[A](value: A)

  implicit val stringPrintable = new Printable[String] {
    def format(a: String): String = s""""$a""""
  }

  implicit val booleanPrintable = new Printable[Boolean] {
    def format(value: Boolean): String =
      if(value) "yes" else "no"
  }

  implicit def boxPrintable[A](implicit p: Printable[A]) = p.contramap[Box[A]](_.value)

  "printable itself" in {
    format("hello") mustBe "\"hello\""
    format(true) mustBe "yes"
  }

  "printable contramap" in {
    format(Box("hello world")) mustBe "\"hello world\""
    format(Box(true)) mustBe "yes"
  }

}
