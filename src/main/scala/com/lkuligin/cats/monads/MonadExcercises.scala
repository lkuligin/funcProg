package com.lkuligin.cats.monads

import cats.{Eval, Monad}
import cats.data.Writer
import cats.syntax.applicative._
import cats.syntax.writer._
import cats.instances.vector._
import cats.syntax.flatMap._
import cats.syntax.functor._

object MonadExcercises {
  def foldRightSafe[A, B](as: List[A], acc: Eval[B])(fn: (A, Eval[B]) => Eval[B]): Eval[B] =
    as match {
      case head :: tail =>
        Eval.defer(fn(head, foldRightSafe(tail, acc)(fn)))
      case Nil =>
        acc
    }

  type Logged[A] = Writer[Vector[String], A]

  def slowly[A](body: => A) =
    try body finally Thread.sleep(100)

  def factorial(n: Int): Logged[Int] =
    for {
      ans <- if (n == 0) 1.pure[Logged] else slowly(factorial(n - 1).map(_ * n))
      _ <- Vector(s"fact $n $ans").tell
    } yield ans

  def product[M[_] : Monad, A, B](fa: M[A], fb: M[B]): M[(A, B)] = fa.flatMap(a => fb.map(b => (a, b)))

}
