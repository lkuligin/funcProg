package com.lkuligin.funprog

/**
  * Created by lkuligin on 28/06/2017.
  */


object Ch4 {
  sealed trait MyOption[+A] {
    def map[B](f: A => B): MyOption[B] = this match {
      case MySome(value) => MySome(f(value))
      case _ => MyNone
    }

    def flatMap[B](f: A => MyOption[B]): MyOption[B] = this match {
      case MySome(value) => f(value)
      case _ => MyNone
    }

    def getOrElse[B >: A](default: => B): B = this match {
      case MySome(value) => value
      case _ => default
    }

    def orElse[B >: A](ob: => MyOption[B]): MyOption[B] = this match {
      case MySome(value) => this
      case _ => ob
    }

    def filter(f: A => Boolean): MyOption[A] = this match {
      case MySome(value) if f(value) => MySome(value)
      case _ => MyNone
    }

  }

  case class MySome[+A](get: A) extends MyOption[A]
  case object MyNone extends MyOption[Nothing]

  /**
    * 4.2. Implement variance in terms of flatMap
    */
  def variance(xs: Seq[Double]): MyOption[Double] =
    if (xs.isEmpty) MyNone else MySome(xs.map(x => x*x).sum/xs.length)

  /**
    * 4.3 combines two Option values using a binary function
    */
  def map2[A,B,C](a: MyOption[A], b: MyOption[B])(f: (A,B) => C): MyOption[C] = (a,b) match {
    case (MyNone, _) | (_, MyNone) => MyNone
    case (MySome(val1), MySome(val2)) => MySome(f(val1, val2))
  }

  /**
    * 4.4 combines a list of Options into one Option containing a list of all values
    */
  def sequence[A](a: List[MyOption[A]]): MyOption[List[A]] =
    a.foldRight(MySome(List[A]()): MyOption[List[A]])(
      (maybeEl, maybeAcc) => (maybeEl, maybeAcc) match {
        case (MySome(el), MySome(lst)) => MySome(el :: lst)
        case _ => MyNone
      }
    )

  /**
    * 4.5 map over a list with function that might return None
    */
  def traverse[A, B](a: List[A])(f: A => MyOption[B]): MyOption[List[B]] =
    a.foldRight(MySome(List[B]()): MyOption[List[B]])(
      (el, maybeAcc) => (maybeAcc, f(el)) match {
        case (MyNone, _) => MyNone
        case (_, MyNone) => MyNone
        case (MySome(lst), MySome(res)) => MySome(res :: lst)
      }
    )

  sealed trait MyEither[+E, +A] {
    def map[B](f: A => B): MyEither[E, B] = this match {
      case MyLeft(err) => MyLeft(err)
      case MyRight(vl) => MyRight(f(vl))
    }
    def flatMap[EE >: E, B](f: A => MyEither[EE, B]): MyEither[EE, B] = this match {
      case MyRight(vl) => f(vl)
      case MyLeft(err) => MyLeft(err)
    }
    def orElse[EE >: E, B >: A](b: => MyEither[EE, B]): MyEither[EE, B] = this match {
      case MyLeft(err) => b
      case MyRight(vl) => MyRight(vl)
    }
    def map2[EE >: E, B >: A, C](b: MyEither[EE, B])(f: (A,B) => C): MyEither[EE, C] =
      (this, b) match {
        case (MyRight(val1), MyRight(val2)) => MyRight(f(val1, val2))
        case (MyLeft(err), _) => MyLeft(err)
        case (_, MyLeft(err)) => MyLeft(err)
      }
  }

  case class MyLeft[+E](value: E) extends MyEither[E, Nothing]
  case class MyRight[+A](value: A) extends MyEither[Nothing, A]

  //def MyTry[A](a: => A): Either[Exception, A]

  /**
    * 4.7 Implement sequence and traverse for Either
    */
  def sequenceEither[E, A](es: List[MyEither[E,A]]): MyEither[E, List[A]] =
    es.foldRight(MyRight(List()): MyEither[E, List[A]])(
      (el, maybeAcc) => (el, maybeAcc) match {
        case (MyLeft(err), _) => MyLeft(err)
        case (_, MyLeft(err)) => MyLeft(err)
        case (MyRight(val1), MyRight(lst)) => MyRight(val1 :: lst)
      }
    )

  def traverseEither[E, A, B](as: List[A])(f: A => MyEither[E,B]): MyEither[E, List[B]] =
    as.foldRight(MyRight(List()): MyEither[E, List[B]])(
      (el, maybeAcc) => (f(el), maybeAcc) match {
        case (MyLeft(err), _) => MyLeft(err)
        case (_, MyLeft(err)) => MyLeft(err)
        case (MyRight(val1), MyRight(lst)) => MyRight(val1 :: lst)
      }
    )


}
