package com.lkuligin.cats.Intro

object PrintableSyntax {
  implicit class PrintOps[A](value: A) {
    def format(implicit p: Printable[A]): String =
      p.format(value)
    def print(implicit p: Printable[A]): Unit =
      println(p.format(value))
  }
}
