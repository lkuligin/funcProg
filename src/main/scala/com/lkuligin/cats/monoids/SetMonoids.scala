package com.lkuligin.cats.monoids

object SetMonoids {

  def unionMonoid[A]: MyMonoid[Set[A]] = new MyMonoid[Set[A]] {
    def combine(x: Set[A], y: Set[A]): Set[A] = x | y

    def empty: Set[A] = Set[A]()
  }

  def intersectSemiGroup[A]: MySemiGroup[Set[A]] = (x: Set[A], y: Set[A]) => x & y

}
