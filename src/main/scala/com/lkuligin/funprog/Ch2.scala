package com.lkuligin.funprog

/**
  * Created by lkuligin on 20/06/2017.
  */
class Ch2 {
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


}
