package com.lkuligin.funprog.Ch10


/**
  * 10.12 implement Foldable[Stream]
  */

object StreamFoldable extends Foldable[Stream] {
  def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)

  def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)
}