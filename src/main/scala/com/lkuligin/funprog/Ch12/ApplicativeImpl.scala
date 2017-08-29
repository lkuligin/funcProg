package com.lkuligin.funprog.Ch12

import com.lkuligin.funprog.Ch11.Monad

object ApplicativeImpl {

  val streamApplicative = new Applicative[Stream] {
    override def unit[A](a: => A): Stream[A] = Stream(a)

    override def map2[A, B, C](fa: Stream[A], fb: Stream[B])(f: (A, B) => C): Stream[C] = fa.zip(fb).map(f.tupled)
  }

  val optionApplicative = new Applicative[Option] {
    override def unit[A](a: => A): Option[A] = Some(a)

    override def map2[A, B, C](fa: Option[A], fb: Option[B])(f: (A, B) => C): Option[C] = (fa, fb) match {
      case (Some(a), Some(b)) => Some(f(a,b))
      case _ => None
    }
  }

  /**
    * 12.5 write a monad instance for Either
    */
  def eitherMonad[E]: Monad[({type f[x] = Either[E, x]})#f] = new Monad[({type f[x] = Either[E, x]})#f] {
    override def unit[A](a: => A): Either[E, A] = Right(a)

    override def flatMap[A, B](ma: Either[E, A])(f: (A) => Either[E, B]): Either[E, B] = ma match {
      case Left(e) => Left(e)
      case Right(e) => f(e)
    }
  }

  /**
    * 12.6 writhe an applicative instance for Validation that accumulates errors in Failure
    */
  sealed trait Validation[+E, +A]

  case class Failure[E](head: E, tail: Vector[E])
    extends Validation[E, Nothing]

  case class Success[A](a: A) extends Validation[Nothing, A]

  def validationApplicative[E]: Applicative[({type f[x] = Validation[E,x]})#f] =  new Applicative[({type f[x] = Validation[E,x]})#f] {
    override def unit[A](a: => A): Validation[E, A] = Success(a)

    override def map2[A, B, C](fa: Validation[E, A], fb: Validation[E, B])(f: (A, B) => C): Validation[E, C] = (fa, fb) match {
      case (Success(a), Success(b)) => Success(f(a,b))
      case (Failure(head1, tail1), Failure(head2, tail2)) => Failure(head1, tail1 ++ Vector(head2) ++ tail2)
      case (e@Failure(_, _), _) => e
      case (_, e@Failure(_, _)) => e
    }
  }

}
