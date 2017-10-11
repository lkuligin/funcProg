package com.lkuligin.cats.monoids

trait MySemiGroup[A] {
  def combine(x: A, y: A): A
}

object MySemiGroup {
  def apply[A](implicit semiGroup: MySemiGroup[A]) = semiGroup
}