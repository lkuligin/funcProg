package com.lkuligin.funprog.Ch10

trait Foldable[F[_]] {
  def foldRight[A,B](as: F[A])(z: B)(f: (A,B) => B): B
  def foldLeft[A,B](as: F[A])(z: B)(f: (B,A) => B): B
  def foldMap[A,B](as: F[A])(z: B)(f: A => B)(m: Monoid[B]): B = foldRight(as)(z)((x,y) => m.op(f(x),y))
  def concatenate[A](as: F[A])(m: Monoid[A]): A = foldLeft(as)(m.zero)(m.op)

  /**
    * 10.15 implement toList
    */
  def toList[A](fs: F[A]): List[A] = foldRight(fs)(List[A]())(_ :: _)
}
