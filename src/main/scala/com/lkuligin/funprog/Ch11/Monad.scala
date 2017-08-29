package com.lkuligin.funprog.Ch11

trait Monad[F[_]] extends Functor[F] {
  def unit[A](a: => A): F[A]

  def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B]

  override def map[A, B](ma: F[A])(f: A => B): F[B] = flatMap(ma)(x => unit(f(x)))

  def map2[A, B, C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

  /**
    * 11.3 implement sequence and traverse combinators
    */
  def sequence[A](lma: List[F[A]]): F[List[A]] = lma.foldRight(unit(List[A]()))((el, acc) => map2(el, acc)(_ :: _))
  def traverse[A,B](la: List[A])(f: A => F[B]): F[List[B]] = la.foldRight(unit(List[B]()))((el, acc) => map2(f(el), acc)(_ :: _))

  /**
    * 11.4 implement replicate monad
    */
  def replicateM[A](n: Int, ma: F[A]): F[List[A]] = sequence(List.fill(n)(ma))

  /**
    * 11.6 implement filter monad
    */

  def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] =
    ms.foldRight(unit(List[A]()))((el,acc) =>
      compose(f, (b: Boolean) => if (b) map2(unit(el),acc)(_ :: _) else acc)(el))

  /**
    * 11.7 implement Kleisli composition
    */
  def compose[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)

  /**
    * 11.8 implement flatMap in terms of compose
    */
  def flatMap2[A, B](ma: F[A])(f: A => F[B]): F[B] = compose((_: Unit) => ma, f)(())

  /**
    * 11.12 implement join in terms of flatMap
    */
  def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(x => x)

  /**
    * 11.13 implement flatMap in terms of join
    */
  def flatMap3[A, B](ma: F[A])(f: A => F[B]): F[B] = join(map(ma)(f))

}

