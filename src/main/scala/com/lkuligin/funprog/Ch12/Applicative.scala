package com.lkuligin.funprog.Ch12

import com.lkuligin.funprog.Ch11.Functor

trait Applicative [F[_]] extends Functor[F] {
  def map2[A,B,C](fa: F[A], fb: F[B])(f: (A,B) => C): F[C]
  def unit[A](a: => A): F[A]

  def map[A, B](fa: F[A])(f: A => B): F[B] = map2(fa, unit(()))((a,_) => f(a))

  def traverse[A,B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List[B]()))((el, acc) => map2(f(el), acc)(_ :: _))

  /**
    * 12.1 implement sequence, replicateM, product
    */
  def sequence[A](fas: List[F[A]]): F[List[A]] = traverse(fas)(x => x)
  def replicateM[A](n: Int, ma: F[A]): F[List[A]] = sequence(List.fill(n)(ma))
  def product[A,B](fa: F[A], fb: F[B]): F[(A,B)] = map2(fa, fb)((x,y) => (x,y))

  /**
    * 12.2 define map2 and map in terms of unit and apply
    */
  def apply[A,B](fab: F[A => B])(fa: F[A]): F[B] = map2(fab, fa)(_(_))

  def map_[A, B](fa: F[A])(f: A => B): F[B] = apply(unit[A => B](f))(fa)
  def map2_[A,B,C](fa: F[A], fb: F[B])(f: (A,B) => C): F[C] = {
    val l: F[(B) => C] = apply(unit[A => B => C]({ a: A => { b: B => f(a, b)}}))(fa)
    apply(l)(fb)
  }

  /**
    * 12.3 implement map3 and map4 using only unit, apply, and the curried method available on functions
    */
  def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] = {
    val l1: F[(B) => (C) => D] = apply(unit[A => B => C => D](f.curried))(fa)
    val l2: F[(C) => D] = apply(l1)(fb)
    apply(l2)(fc)
  }
  def map4[A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => E): F[E] = {
    val l1: F[(B) => (C) => (D) => E] = apply(unit[A => B => C => D => E](f.curried))(fa)
    val l2: F[(C) => (D) => E] = apply(l1)(fb)
    val l3: F[(D) => E] = apply(l2)(fc)
    apply(l3)(fd)
  }

  /**
    * 12.8 implement product of two applicative functors
    */
  def product[G[_]](G: Applicative[G]): Applicative[({type f[x] = (F[x], G[x])})#f] = {
    val self = this

    new Applicative[({type f[x] = (F[x], G[x])})#f] {
      override def unit[A](a: => A): (F[A], G[A]) = (self.unit(a), G.unit(a))

      override def map2[A, B, C](fa: (F[A], G[A]), fb: (F[B], G[B]))(p: (A, B) => C): (F[C], G[C]) =
        (self.map2(fa._1, fb._1)(p), G.map2(fa._2, fb._2)(p))

    }
  }

  /**
    * 12.9 implement compose for applicative functors
    */
  def compose[G[_]](G: Applicative[G]): Applicative[({type f[x] = F[G[x]]})#f] = {
    val self = this

    new Applicative[({type f[x] = F[G[x]]})#f] {
      override def unit[A](a: => A): F[G[A]] = self.unit(G.unit(a))

      override def map2[A, B, C](fa: F[G[A]], fb: F[G[B]])(p: (A, B) => C): F[G[C]] =
        self.map2(fa, fb)(G.map2(_, _)(p))
    }

  }

  /**
    * 12.12 implement sequence over a Map
    */
  def sequenceMap[K,V](ofa: Map[K, F[V]]): F[Map[K, V]] = ofa.foldLeft(unit(Map[K,V]()))( {
    case (acc, (k, v)) => map2(acc, v)((m,vl) => m + (k -> vl))
  })


}
