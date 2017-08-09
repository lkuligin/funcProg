package com.lkuligin.funprog.Ch10

/**
  * 10.14 implement Foldable[Option]
  */

object OptionFoldable extends Foldable[Option] {
  def foldRight[A,B](as: Option[A])(z: B)(f: (A,B) => B): B = as match {
    case Some(v) => f(v,z)
    case _ => z
  }

  def foldLeft[A,B](as: Option[A])(z: B)(f: (B,A) => B): B = as match {
    case Some(v) => f(z,v)
    case _ => z
  }
}
