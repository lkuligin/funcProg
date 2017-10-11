package com.lkuligin.cats.monads

import cats.Monad

object TreeMonad {
  sealed trait Tree[+A]
  final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
  final case class Leaf[A](value: A) extends Tree[A]

  def branch[A](left: Tree[A], right: Tree[A]): Tree[A] = Branch(left, right)
  def leaf[A](value: A): Tree[A] = Leaf(value)

  implicit val treeMonad: Monad[Tree] = new Monad[Tree] {
    override def pure[A](x: A): Tree[A] = leaf[A](x)

    override def flatMap[A, B](fa: Tree[A])(f: (A) => Tree[B]): Tree[B] = fa match {
      case Branch(left, right) => Branch[B](flatMap(left)(f), flatMap(right)(f))
      case Leaf(value) => f(value)
    }

    override def tailRecM[A, B](a: A)(f: (A) => Tree[Either[A, B]]): Tree[B] = f(a) match {
      case Branch(left, right) => Branch(
        flatMap(left) {
          case Left(x) => tailRecM(x)(f)
          case Right(x) => pure(x)
        },
        flatMap(right) {
          case Left(x) => tailRecM(x)(f)
          case Right(x) => pure(x)
        })

      case Leaf(Left(value)) => tailRecM(value)(f)
      case Leaf(Right(value)) => Leaf(value)
    }
  }

}



