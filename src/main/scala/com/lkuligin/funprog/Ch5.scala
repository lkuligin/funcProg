package com.lkuligin.funprog

import com.lkuligin.funprog.Ch5.MyStream

import scala.collection.immutable.Stream.{Empty, cons}

/**
  * Created by lkuligin on 02/07/2017.
  */
object Ch5 {

  sealed trait MyStream[+A] {

    def headOption: Option[A] = this match {
      case MyEmpty => None
      case MyCons(h, t) => Some(h())
    }

    /**
      *5.1 converts a Stream to List, which forces evaluation
      */
    def toList: List[A] = this match {
      case MyEmpty => List()
      case MyCons(h, t) => h() :: t().toList
    }

    /**
      * 5.2 returns the first n elements of a Stream
      */
    def take(n: Int): MyStream[A] =
      if (n <= 0) MyEmpty else this match {
        case MyEmpty => MyEmpty
        case MyCons(h, t) => MyCons(h, () => t().take(n - 1))
      }

    def drop(n: Int): MyStream[A] =
      if (n <= 0) this else this match {
        case MyEmpty => MyEmpty
        case MyCons(h, t) => t().drop(n - 1)
      }

    /**
      * 5.3 returns all starting elements of a Stream that match the given predicate
      */
    def takeWhile(predicate: A => Boolean): MyStream[A] = this match {
      case MyEmpty => MyEmpty
      case MyCons(h, t) if predicate(h()) => MyCons(h, () => t().takeWhile(predicate))
      case MyCons(_, t) => t().takeWhile(predicate)
    }

    def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
      case MyCons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

    /**
      * 5.4 checks that all elements in the Stream match a given predicate
      */
    def forAll(p: A => Boolean): Boolean = this.foldRight(true)((a, b) => b && p(a))

    /**
      * 5.5 use foldRight to implement takeWhile
      */
    def takeWhile2(p: A => Boolean): MyStream[A] = this.foldRight(MyEmpty: MyStream[A])((el, acc) => {
      if (p(el)) MyStream.cons(el, acc) else MyEmpty
    })

    /**
      * 5.6 use foldRight to implement headOption
      */
    def headOption2: Option[A] = this.foldRight(None: Option[A])((el, _) => Some(el))

    /**
      * 5.7 Implement map, filter, append, flatMap using foldRight
      */
    def map[B](f: A => B): MyStream[B] = this.foldRight(MyEmpty: MyStream[B])((el, acc) => MyStream.cons(f(el), acc))

    def filter(p: A => Boolean): MyStream[A] = this.foldRight(MyEmpty: MyStream[A])((el, acc) => {
      if (p(el)) MyStream.cons(el, acc) else acc
    })

    def append[AA >: A](ns: MyStream[AA]): MyStream[AA] = this.foldRight(ns)(MyStream.cons(_, _))

    def flatMap[B](f: A => MyStream[B]): MyStream[B] = this.foldRight(MyEmpty: MyStream[B])((el, acc) => f(el).append(acc))

  }

  /**
    * 5.8 returns an infinite Stream of a given value
    */
  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  /**
    * 5.9 generates an infinite Stream of integers n. n+1, ...
    */
  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  /**
    * 5.10 generates an infinite Stream of Fibonacci numbers 0,1,1,2,3,5,8
    */
  def fib: Stream[Int] = {
    def acc(el: Int, elNext: Int): Stream[Int] = cons(el, acc(elNext, elNext + el))

    acc(0, 1)
  }

  /**
    * 5.11 builds stream from initial state and a function for producing both the next state and the next value
    */
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((value, state)) => cons(value, unfold(state)(f))
    case _ => Empty
  }

  /**
    * 5.12 fibs, from, constant with unfold
    */
  def constant2[A](a: A): Stream[A] = unfold[A, Boolean](true)(_ => Some((a, true)))

  def from2(n: Int): Stream[Int] = unfold[Int, Int](n)(x => Some(x, x + 1))

  def fib2: Stream[Int] = unfold[Int, (Int, Int)]((0, 1))(x => Some(x._1, (x._2, x._1 + x._2)))

  /**
    * 5.13 use unfold to implement map, take, takeWhile
    */
  def map2[A, B](stream: Stream[A])(f: A => B): Stream[B] = unfold[B, Stream[A]](stream)(x => x.headOption.map(y => (f(y), x.tail)))

  def take2[A](stream: Stream[A])(n: Int): Stream[A] = unfold[A, (Int, Stream[A])]((0, stream))
    {x => if (x._1 < n) x._2.headOption.map(y => (y, (x._1+1, x._2.tail))) else None}

  def takeWhile3[A](stream: Stream[A])(p: A => Boolean): Stream[A] = unfold[A, Stream[A]](stream)
    { x => x.headOption.filter(p).map((_, x.tail)) }

  def zip(stream1: Stream[Int], stream2: Stream[Int]): Stream[Int] = unfold[Int, (Stream[Int], Stream[Int])]((stream1, stream2))
    {x => x._1.headOption.flatMap(y => x._2.headOption.map(_ + y)).map((_, (x._1.tail, x._2.tail)))}

  def zipAll2[A, B](s1: Stream[A])(s2: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold[(Option[A], Option[B]), (Stream[A], Stream[B])]((s1, s2))
      { x => if (x._1.isEmpty && x._2.isEmpty) None
        else Some((x._1.headOption, x._2.headOption), (if (x._1.isEmpty) Empty else x._1.tail, if (x._2.isEmpty) Empty else x._2.tail)) }

  /**
    * 5.14 checks if one Stream is a prefix of another
    */
  def startsWith[A](s1: Stream[A])(s2: Stream[A]): Boolean = {
    zipAll2(s1)(s2)
      .map(x => x._2.map(y => x._1.map(_ == y).getOrElse(false)))
      .takeWhile(_.map(_ => true).getOrElse(false))
      .foldRight(true)((x, el) => x.getOrElse(true) && el)
  }

  /**
    * 5.15 returns the Stream of suffixes of the input sequence
    */
  def tails[A](s: Stream[A]): Stream[Stream[A]] =
    unfold(s)(x => if (x.isEmpty) None else Some(x, x.tail))

  /**
    * 5.16 returns a Stream of the intermediate results
    */
  def scanRight[A,B](s: Stream[A])(z: => B)(f: (A, => B) => B): Stream[B] = {
    s.foldRight((z, Stream(z)))((el, accPair) => {
      val b: B = f(el, accPair._1)
      (b, cons(b, accPair._2))
    })._2
  }

  case object MyEmpty extends MyStream[Nothing]
    case class MyCons[+A](h: () => A, t: () => MyStream[A]) extends MyStream[A]

    object MyStream {
      def cons[A](hd: => A, tl: => MyStream[A]): MyStream[A] = {
        lazy val head = hd
        lazy val tail = tl
        MyCons(() => head, () => tail)
      }

      def empty[A]: MyStream[A] = MyEmpty

      def apply[A](as: A*): MyStream[A] =
        if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
    }
}
