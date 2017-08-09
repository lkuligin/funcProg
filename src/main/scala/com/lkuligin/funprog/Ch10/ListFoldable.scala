package com.lkuligin.funprog.Ch10

/**
  * 10.12 implement Foldable[List]
  */
object ListFoldable extends Foldable[List] {
  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)

  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)

  override def toList[A](as: List[A]): List[A] = as
}
