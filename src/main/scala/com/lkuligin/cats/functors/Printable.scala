package com.lkuligin.cats.functors

trait Printable[A] {
  def format(a: A): String

  def contramap[B](f: B => A): Printable[B] = {
    val self = this
    (b: B) => self.format(f(b))
  }
}

object Printable {
  def format[A](input: A)(implicit p: Printable[A]): String = p.format(input)
}