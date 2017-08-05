package com.lkuligin.funprog.Ch8

case class SGen[A](forSize: Int => Gen[A]) {
  /**
    * 8.11 define some convenience functions and delegate them to Gen
    */
  def apply(n: Int): Gen[A] = forSize(n)

  def flatMap[B](f: A => SGen[B]): SGen[B] = {
    val a: Int => Gen[B] = x => forSize(x).flatMap(f(_).forSize(x))
    SGen(a)
  }
}
