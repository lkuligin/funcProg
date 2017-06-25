package com.lkuligin.funprog

import org.scalatest.{MustMatchers, WordSpec}
import org.mockito.Matchers._
import org.scalatest.mock.MockitoSugar
import com.lkuligin.funprog.Ch2

import scala.util.Random

/**
  * Created by lkuligin on 20/06/2017.
  */
class Ch2Spec extends WordSpec with MustMatchers with TestHelper {
  val o = Ch2

  "fib" when {
    "run" should {
      "throw exception if negative argument" in {
        assertThrows[IllegalArgumentException]{o.fib(-1)}
        assertThrows[IllegalArgumentException]{o.fib(0)}
      }
      "return correct values" in {
        o.fib(1) mustBe 1
        o.fib(2) mustBe 1
        o.fib(3) mustBe 2
        o.fib(4) mustBe 3
      }
    }
  }

  "isSorted" when {
    "invoked" should {
      "return true for empty array" in {
        o.isSorted[Int](Array[Int](), (a: Int, b: Int) => true) mustBe true
        o.isSorted[Int](Array[Int](), (a: Int, b: Int) => false) mustBe true
      }
      "return true for array length 1" in {
        o.isSorted[Int](Array[Int](1), (a: Int, b: Int) => true) mustBe true
        o.isSorted[Int](Array[Int](1), (a: Int, b: Int) => false) mustBe true
      }
      "works properly" in {
        o.isSorted[Int](Array[Int](1, 2, 3, 4), (a: Int, b: Int) => a > b) mustBe false
        o.isSorted[Int](Array[Int](1, 2, 3, 4), (a: Int, b: Int) => a < b) mustBe true
        o.isSorted[Int](Array[Int](1, 1, 2, 3, 4), (a: Int, b: Int) => a <= b) mustBe true
        o.isSorted[String](Array[String]("a", "aa", "aaa", "aaaa"), (a: String, b: String) => a.length <= b.length) mustBe true
      }
      "linear on array size" in {
        val res = List(4,5,6,7)
          .map(x => {
            val ar: Array[Int] = Seq.fill(Math.pow(10, x).toInt)(Random.nextInt).toArray
            testTime({o.isSorted(ar, (a: Int, b: Int) => a > b)})
          })
        println("isSorted nanoseconds depending on exp-increasing array", res)
      }
    }
  }

  "currying" when {
    "invoked" should {
      "work" in {
        1 mustBe 1
      }
    }
  }
}
