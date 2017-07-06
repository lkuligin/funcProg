package com.lkuligin.funprog

import com.lkuligin.funprog.Ch5.{MyEmpty, MyStream}
import org.scalatest.{MustMatchers, WordSpec}

import scala.collection.immutable.Stream.{Empty, cons}

/**
  * Created by lkuligin on 02/07/2017.
  */
class Ch5Spec extends WordSpec with MustMatchers with TestHelper {
  val o = Ch5

  "trait MyStream" should {
    "implement toList properly" in {
      MyEmpty.toList mustBe List()
      MyStream(1, 2, 3, 4, 5).toList mustBe List(1, 2, 3, 4, 5)
    }
    "implement take(n) properly" in {
      MyEmpty.take(5) mustBe MyEmpty
      MyStream(1, 2, 3).take(2).toList mustBe List(1, 2)
      MyStream(1, 2, 3).take(4).toList mustBe List(1, 2, 3)
    }
    "implement drop(n) properly" in {
      MyEmpty.drop(5) mustBe MyEmpty
      MyStream(1, 2, 3).drop(1).toList mustBe List(2, 3)
      MyStream(1, 2, 3).drop(2).toList mustBe List(3)
      MyStream(1, 2, 3).drop(4) mustBe MyEmpty
    }
    "implement takeWhile properly" in {
      MyEmpty.takeWhile(_ => true) mustBe MyEmpty
      MyStream(1, 2, 3).takeWhile(x => x < 3).toList mustBe List(1, 2)
      MyStream(1, 2, 3).takeWhile(x => x > 0).toList mustBe List(1, 2, 3)
      MyStream(1, 2, 3).takeWhile(x => x < 0).toList mustBe List()
    }
    "implement forAll properly" in {
      MyEmpty.forAll(_ => true) mustBe true
      MyStream(1, 2, 3).forAll(_ < 3) mustBe false
      MyStream(1, 2, 3).forAll(_ > 0) mustBe true
    }
    "implement takeWhile2 properly" in {
      MyEmpty.takeWhile2(_ => true) mustBe MyEmpty
      MyStream(1, 2, 3).takeWhile2(_ < 3).toList mustBe List(1, 2)
      MyStream(1, 2, 3).takeWhile2(_ > 0).toList mustBe List(1, 2, 3)
      MyStream(1, 2, 3).takeWhile2(_ < 0).toList mustBe List()
    }
    "implement headOption2 properly" in {
      MyEmpty.headOption2 mustBe None
      MyStream(1, 2, 3).headOption2 mustBe Some(1)
      MyStream(1).headOption2 mustBe Some(1)
    }
    "implement map properly" in {
      MyEmpty.map(_ => 1) mustBe MyEmpty
      MyStream(1, 2, 3).map(_ + 1).toList mustBe List(2, 3, 4)
      MyStream("a", "aa", "aaa").map(_.length()).toList mustBe List(1, 2, 3)
    }
    "implement filter properly" in {
      MyEmpty.filter(_ => true) mustBe MyEmpty
      MyStream(1, 2, 3).filter(_ % 2 == 1).toList mustBe List(1, 3)
      MyStream(1, 2, 3).filter(_ < 0) mustBe MyEmpty
    }
    "implement append properly" in {
      MyEmpty.append(MyEmpty) mustBe MyEmpty
      MyStream(1, 2, 3).append(MyEmpty).toList mustBe List(1, 2, 3)
      MyEmpty.append(MyStream(1, 2, 3)).toList mustBe List(1, 2, 3)
      MyStream(1, 2, 3).append(MyStream(4, 5)).toList mustBe List(1, 2, 3, 4, 5)
    }
    "implement flatMap properly" in {
      MyEmpty.flatMap(_ => MyStream(1, 2, 3)) mustBe MyEmpty
      MyStream(1, 2, 3).flatMap(x => MyStream(x, x * 10)).toList mustBe List(1, 10, 2, 20, 3, 30)
      MyStream(1, 2, 3).flatMap(x => MyEmpty) mustBe MyEmpty
    }
  }

  "constant" should {
    "return an infinite stream of constant" in {
      o.constant(1).take(5).toList mustBe List(1, 1, 1, 1, 1)
      o.constant("a").take(2).toList mustBe List("a", "a")
    }
    "do the same with unfold" in {
      o.constant2(1).take(5).toList mustBe List(1,1,1,1,1)
      o.constant2("a").take(2).toList mustBe List("a","a")
    }
  }
  "from" should {
    "return an infinite stream of integers" in {
      o.from(1).take(5).toList mustBe List(1, 2, 3, 4, 5)
      o.from(-2).take(2).toList mustBe List(-2, -1)
    }
    "do the same with unfold" in {
      o.from2(1).take(5).toList mustBe List(1, 2, 3, 4, 5)
      o.from2(-2).take(2).toList mustBe List(-2, -1)
    }
  }
  "fib" should {
    "return an infinite stream of Fibonacci numbers" in {
      o.fib.take(7).toList mustBe List(0, 1, 1, 2, 3, 5, 8)
    }
    "do the same with unfold" in {
      o.fib2.take(7).toList mustBe List(0, 1, 1, 2, 3, 5, 8)
    }
  }
  "map2" should {
    "work properly" in {
      o.map2(Empty)(_ => 1) mustBe Empty
      o.map2(Stream(1, 2, 3))(_ + 1).toList mustBe List(2, 3, 4)
      o.map2(Stream("a", "aa", "aaa"))(_.length()).toList mustBe List(1, 2, 3)
    }
  }
  "take2(n)" should {
    "work properly" in {
      o.take2(Empty)(5) mustBe Empty
      o.take2(Stream(1, 2, 3))(2).toList mustBe List(1, 2)
      o.take2(Stream(1, 2, 3))(4).toList mustBe List(1, 2, 3)
    }
  }
  "takeWhile with unfold" should {
    "work properly" in {
      o.takeWhile3(Empty)(_ => true) mustBe Empty
      o.takeWhile3(Stream(1, 2, 3))(_ < 3).toList mustBe List(1, 2)
      o.takeWhile3(Stream(1, 2, 3))(_ > 0).toList mustBe List(1, 2, 3)
      o.takeWhile3(Stream(1, 2, 3))(_ < 0).toList mustBe List()
    }
  }
  "zip" should {
    "work properly" in {
      o.zip(Empty, Stream(1,2,3)) mustBe Empty
      o.zip(Stream(1,2,3), Stream(2,3,4,5)).toList mustBe List(3,5,7)
    }
  }
  "zipAll" should {
    "work properly" in {
      o.zipAll2(Empty)(Stream(1,2,3)).toList mustBe List((None, Some(1)), (None, Some(2)), (None, Some(3)))
      o.zipAll2(Stream(1,2))(Stream(2,3,4)).toList mustBe List((Some(1), Some(2)), (Some(2), Some(3)), (None, Some(4)))
    }
  }
  "startsWith" should {
    "work properly" in {
      o.startsWith(Stream(1,2,3))(Empty) mustBe true
      o.startsWith(Stream(1,2,3))(Stream(1,2)) mustBe true
      o.startsWith(Stream(1,2,3))(Stream(4,2)) mustBe false
      o.startsWith(Stream(1,2,3))(Stream(1,2,3,4)) mustBe false
    }
  }
  "tails" should {
    "work properly" in {
      o.tails(Stream(1,2,3)).toList.map(_.toList) mustBe List(List(1,2,3), List(2,3), List(3))
      o.tails(Empty) mustBe Empty
    }
  }
  "scanRight" should {
    "work properly" in {
      o.scanRight(Stream(1,2,3))(0)(_+_).toList mustBe Stream(1,2,3).scanRight(0)(_+_).toList //List(6, 5, 3, 0)
    }
    "scan the list only once" in {
      var scans: Int = 0
      def testHelper(i: Int): Int = {
        scans += 1
        i
      }
      val a = cons(testHelper(1), cons(testHelper(2), cons(testHelper(3), Empty)))
      val b = o.scanRight(a)(0)(_+_).toList
      scans mustBe 3
    }
  }
}
