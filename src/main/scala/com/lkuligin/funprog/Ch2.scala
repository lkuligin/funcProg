package com.lkuligin.funprog

/**
  * Created by lkuligin on 20/06/2017.
  */
object Ch2 {
  /**
  2.1 recurisive function to get the nth Fibonacci number
  */
  def fib(n: Int) = {
    @annotation.tailrec
    def go(i: Int, acc1: Int, acc2: Int): Int = {
      if (i==0) acc1
      else go(i-1, acc1+acc2, acc1)
    }
    n match {
      case n if n <= 0 => throw new IllegalArgumentException("fibonacci argument should be greater than 0!")
      case n if n <= 2 => 1
      case n => go(n-2, 1, 1)
    }
  }

  /**
    2.2 implement isSorted which checks whether an Array[A] is sorted acc. to a given comparison func
    */
  def isSorted[A](arr: Array[A], ordered: (A,A) => Boolean): Boolean = {
    @annotation.tailrec
    val length = arr.length
    def loop(el: A, pos: Int): Boolean =
      if (pos >= length - 1) true
      else {
        lazy val nextEl = arr(pos+1)
        ordered(el, nextEl) && (loop(nextEl, pos+1))
      }

    if (arr.isEmpty || arr.length == 1) true
    else loop(arr(0), 1)
  }

  /**
    2.3 currying converts a func of 2 arguments into
    a function of one argument that partially apples f
    */
  def curry[A,B,C](f: (A,B) => C): A => (B => C) =
    (a:A) => (b:B) => f(a,b)

  /**
    2.4 uncurrying reverses transormation of curry
    */
  def uncurry[A,B,C](f: A => B => C): (A,B) => C =
    (a: A, b: B) => f(a)(b)

  /**
    2.5 functions composition
    */
  def compose[A,B,C](f: B => C, g: A => B): A => C =
    (a:A) => f(g(a))

}
