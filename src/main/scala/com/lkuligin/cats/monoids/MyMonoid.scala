package com.lkuligin.cats.monoids

trait MyMonoid[A] extends MySemiGroup[A] {
  def empty: A
}

object MyMonoid {
  def apply[A](implicit monoid: MyMonoid[A]) =
    monoid
}