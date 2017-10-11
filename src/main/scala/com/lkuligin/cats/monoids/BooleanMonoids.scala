package com.lkuligin.cats.monoids

object BooleanMonoids {

  val andMonoid: MyMonoid[Boolean] = new MyMonoid[Boolean] {
      def combine(x: Boolean, y: Boolean) = x && y
      def empty = true
    }

  val orMonoid: MyMonoid[Boolean] = new MyMonoid[Boolean] {
    def combine(x: Boolean, y: Boolean) = x || y
    def empty = false
  }

  val xorMonoid: MyMonoid[Boolean] = new MyMonoid[Boolean] {
    def combine(x: Boolean, y: Boolean): Boolean = (x || y) & !(x && y)
    def empty = false
  }

  val xnorMonoid: MyMonoid[Boolean] = new MyMonoid[Boolean] {
    def combine(x: Boolean, y: Boolean): Boolean = !xorMonoid.combine(x,y)
    def empty = true
  }
}
