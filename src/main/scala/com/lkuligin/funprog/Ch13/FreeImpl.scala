package com.lkuligin.funprog.Ch13

import com.lkuligin.funprog.Ch11.Monad
import com.lkuligin.funprog.Ch13._

object FreeImpl {
  /**
    * 13.1 implement map and flatMap methods on Free trait and give the Monad interface for Free[F, _]
    */
  def freeMonad[F[_]]: Monad[({type f[a] = Free[F,a]})#f] = new Monad[({type f[a] = Free[F,a]})#f] {
    override def unit[A](a: => A): Free[F, A] = Return(a)

    override def flatMap[A, B](ma: Free[F, A])(f: (A) => Free[F, B]): Free[F, B] = ma.flatMap(f)
  }
}
