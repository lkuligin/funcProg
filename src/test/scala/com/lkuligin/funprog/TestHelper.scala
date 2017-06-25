package com.lkuligin.funprog

/**
  * Created by lkuligin on 21/06/2017.
  */
trait TestHelper {
  def time[R](block: => R): R = {
    val startTime = System.nanoTime()
    val result = block
    val endTime = System.nanoTime()
    println("Elapsed time: " + (endTime - startTime) + "ns")
    result
  }

  def testTime(block: Unit): Long = {
    val startTime = System.nanoTime()
    val result = block
    val endTime = System.nanoTime()
    endTime - startTime
  }

  val l = List
}
