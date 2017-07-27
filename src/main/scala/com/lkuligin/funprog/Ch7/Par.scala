package com.lkuligin.funprog.Ch7

import java.util.concurrent.{Callable, ExecutorService, Future, TimeUnit}
import language.implicitConversions

object Par {
  type Par[A] = ExecutorService => Future[A]

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true

    def get(timeout: Long, units: TimeUnit) = get

    def isCancelled = false

    def cancel(eventIfRunning: Boolean): Boolean = false
  }

  def fork[A](a: => Par[A]): Par[A] = es => es.submit(new Callable[A]{
    def call = a(es).get
  })

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = {
    es: ExecutorService => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get))
    }
  }

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  /**
    * 7.3 Fix the implementation of map2 so that it respects the contract of timeouts on Future
    */
  def map2t[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = {
    es: ExecutorService => {
      val (af, bf) = (a(es), b(es))
      new Future[C] {
        override def cancel(mayInterruptIfRunning: Boolean): Boolean = true

        override def isCancelled: Boolean = af.isCancelled || bf.isCancelled

        override def isDone: Boolean = af.isDone || bf.isDone

        override def get(): C = f(af.get, bf.get)

        override def get(timeout: Long, unit: TimeUnit): C = {
          val startTime: Long = System.currentTimeMillis
          val a = af.get(timeout, unit)

          val remainingTime: Long = unit.toMillis(timeout) - (System.currentTimeMillis() - startTime)
          val b = bf.get(remainingTime, unit)

          f(a,b)
        }
      }
    }
  }

  /**
    * 7.4 Using lazyUnit, write a function to convert any function A => B to one that evaluates the result asynchronously
    */

  def asyncF[A,B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  def sortPar(parList: Par[List[Int]]): Par[List[Int]] = map2(parList, unit(()))((a, _) => a.sorted)

  def map[A, B](pa: Par[A])(f: A => B): Par[B] = map2(pa, unit(()))((a, _) => f(a))

  /**
    * 7.5 Write a function that converts List[Pat[A]] to Par[List[A]] without any additional primitives needed
    */
  def sequence[A](ps: List[Par[A]]): Par[List[A]] = ps.foldRight(unit(List[A]()))((lst, p) => map2(p, lst)((x, y) => y :: x))

  /**
    * 7.6 Filters elements in a list in parallel
    */
  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    val lst: List[Par[List[A]]] = as.map {
      asyncF((a: A) => if (f(a)) List(a) else List())
    }
    map(sequence(lst))(_.flatten)
  }

  /**
    * 7.11 Implement choiceN and then choice in terms of choiceN
    */
  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = es => choices(run(es)(n).get)(es)

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = choiceN(map(cond)(if (_) 0 else 1))(List(t,f))

  /**
    * 7.12 Use Map instead of List for choices implementation
    */
  def choiceN2[K,V](key: Par[K])(choices: Map[K,Par[V]]): Par[V] = es => choices(run(es)(key).get)(es)

  /**
    * 7.13 Implement more general function for choice
    */
  def chooser[A,B](pa: Par[A])(choices: A => Par[B]): Par[B] = es => choices(run(es)(pa).get)(es)

  /**
    * 7.14 Combinator for converting a Par[Par[X]] to Par[X] for any choice of X
    */
  def join[A](a: Par[Par[A]]): Par[A] = es => run(es)(a).get()(es)
}