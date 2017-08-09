package com.lkuligin.funprog.Ch10

import com.lkuligin.funprog.Ch10

/**
  * 10.13 implement Foldable for Tree[A]
  */
sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object TreeFoldable extends Foldable[Ch10.Tree] {
  def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B = as match {
    case Leaf(v) => f(v,z)
    case Branch(l: Tree[A],r: Tree[A]) => {
      val right: B = foldRight(r)(z)(f)
      foldRight(l)(right)(f)
    }
  }

  def foldLeft[A,B](as: Tree[A])(z: B)(f: (B,A) => B): B = as match {
    case Leaf(v) => f(z,v)
    case Branch(l: Tree[A],r: Tree[A]) => {
      val left: B = foldLeft(l)(z)(f)
      foldLeft(r)(left)(f)
    }
  }
}
