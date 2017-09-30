package com.lkuligin.cats.Intro

/**
  * Define a type class Printable[A] containing a single method format. format should accept a value of type A and returns a String.
  */


trait Printable[A] {
   def format(a: A): String
}

/**
  * Define an object Printable with two generic interface methods:
  * • format accepts a value of type A and a Printable of the corresponding type. It uses the relevant Printable to convert the A to a String.
  * • print accepts the same parameters as format and returns Unit. It prints the A value to the console using println.
  *
  */
object Printable {
  def format[A](input: A)(implicit p: Printable[A]): String = p.format(input)

  def print[A](input: A)(implicit p: Printable[A]): Unit = {println(format(input))}
}