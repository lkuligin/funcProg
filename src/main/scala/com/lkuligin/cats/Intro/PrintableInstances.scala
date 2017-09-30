package com.lkuligin.cats.Intro


/**
  *   Create an object PrintableInstances containing instances of Printable for String and Int.
  */


object PrintableInstances {
  implicit val stringPrintable = new Printable[String] {
    override def format(a: String): String = a
  }

  implicit val intPrintable = new Printable[Int] {
    override def format(a: Int): String = a.toString
  }
}
