package com.lkuligin.funprog

import org.scalatest.{MustMatchers, WordSpec}
import org.mockito.Matchers._
import org.scalatest.mock.MockitoSugar

/**
  * Created by lkuligin on 20/06/2017.
  */
class Ch2Spec extends WordSpec with MustMatchers {

  val o = new Ch2()

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
}
