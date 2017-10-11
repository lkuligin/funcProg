package com.lkuligin.cats.monads

import scala.language.higherKinds

trait MyMonad[F[_]] {
  def pure[A](a: A): F[A]
  def flatMap[A, B](value: F[A])(func: A => F[B]): F[B]

  def map[A, B](value: F[A])(func: A => B): F[B] =
    flatMap(value)(x => pure(func(x)))

}
