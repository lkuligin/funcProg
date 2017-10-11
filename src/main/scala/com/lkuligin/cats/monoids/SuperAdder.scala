package com.lkuligin.cats.monoids

import cats.syntax.semigroup._
import cats.instances.int._
import cats.Monoid

object SuperAdder {
  def add(items: List[Int]): Int = items.foldLeft(Monoid[Int].empty)(_ |+| _)

  def add[A](items: List[A])(implicit monoid: Monoid[A]): A = items.foldLeft(monoid.empty)(_ |+| _)

  case class Order(totalCost: Double, quantity: Double)

  implicit val monoidOrder: Monoid[Order] = new Monoid[Order] {
    def combine(x: Order, y: Order): Order = Order(x.totalCost+y.totalCost, x.quantity + y.quantity)

    def empty: Order = Order(0, 0)
  }
}
