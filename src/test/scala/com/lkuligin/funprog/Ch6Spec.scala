package com.lkuligin.funprog

import com.lkuligin.funprog.Ch6._
import org.scalatest.{MustMatchers, WordSpec}

/**
  * Created by lkuligin on 07/07/2017.
  */
class Ch6Spec extends WordSpec with MustMatchers with TestHelper {

  val o = Ch6
  val r1 = SimpleRNG(777)
  val r2 = SimpleRNG(1000)

  "nonNegativeInt" should {
    "work properly" in {
      o.nonNegativeInt(r1)._1 mustBe 488231028
      o.nonNegativeInt(r2)._1 mustBe 1861015104
    }
  }

  "double" should {
    "work properly" in {
      o.double(r1)._1 mustBe 0.22735028910792912
      o.double(r2)._1 mustBe 0.8666026894313296
    }
    "same for implementation with map" in {
      o.double2(r1)._1 mustBe 0.22735028910792912
      o.double2(r2)._1 mustBe 0.8666026894313296
    }
  }

  "ints" should {
    "work properly" in {
      o.ints(3)(r1)._1 mustBe List(-952021577, -980842596, 488231028)
      o.ints(2)(r2)._1 mustBe List(-109922977, 1861015104)
    }
  }

  "sequence" should {
    "work properly" in {
      val r: Rand[Int] = _.nextInt
      o.sequence[Int](List(r, r))(SimpleRNG(777))._1 mustBe List(-980842596, 488231028)
    }
  }

  "machine" should {
    "unlock if insert a coin into locked machine and ignore if unlocked" in {
      o.Machine(true, 20, 10).proceed(Coin).locked mustBe false
      o.Machine(true, 20, 10).proceed(Coin).coins mustBe 11
      o.Machine(true, 20, 10).proceed(Coin).candies mustBe 20
      o.Machine(false, 20, 10).proceed(Coin).locked mustBe false
      o.Machine(false, 20, 10).proceed(Coin).coins mustBe 10
      o.Machine(false, 20, 10).proceed(Coin).candies mustBe 20
    }
    "ignore inputing a coin if no candies" in {
      o.Machine(true, 0, 10).proceed(Coin).locked mustBe true
      o.Machine(true, 0, 10).proceed(Coin).coins mustBe 10
      o.Machine(true, 0, 10).proceed(Coin).candies mustBe 0
      o.Machine(false, 0, 10).proceed(Coin).locked mustBe false
      o.Machine(false, 0, 10).proceed(Coin).coins mustBe 10
      o.Machine(false, 0, 10).proceed(Coin).candies mustBe 0
    }
    "give a candy if turning a knob if unlocked and ignore otherwise" in {
      o.Machine(true, 20, 10).proceed(Turn).locked mustBe true
      o.Machine(true, 20, 10).proceed(Turn).coins mustBe 10
      o.Machine(true, 20, 10).proceed(Turn).candies mustBe 20
      o.Machine(false, 20, 10).proceed(Turn).locked mustBe true
      o.Machine(false, 20, 10).proceed(Turn).coins mustBe 10
      o.Machine(false, 20, 10).proceed(Turn).candies mustBe 19
    }
    "run simulation properly" in {
      val inputs = List.fill(4)(List(Coin, Turn)).flatten
      val res = o.simulateMachine(inputs).run(o.Machine(true, 5, 10))
      res._1 mustBe (1,14)
    }
  }

  "State" should {
    val r: State[RNG, Int] = o.State[RNG, Int] (s => s.nextInt)
    val r1 = SimpleRNG(177)
    val r2 = SimpleRNG(217)
    "should implement unit properly" in {
      r.unit(2).run(r1)._1 mustBe 2
    }
  }

  val s: State[RNG, Int] = o.State[RNG, Int] (s => s.nextInt)
  val r = o.SimpleRNG(2).nextInt
  println(r._1)
  println(r._2.nextInt._1)
  println(s
    .flatMap(x => o.State[RNG, Int]( s1 => (s1.nextInt._1 + x, s1.nextInt._2)))
    .run(o.SimpleRNG(2)))

}
