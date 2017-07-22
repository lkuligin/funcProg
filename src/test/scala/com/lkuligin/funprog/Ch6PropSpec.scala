package com.lkuligin.funprog


import com.lkuligin.funprog.Ch6.{RNG, SimpleRNG, State}
import org.scalatest.prop.Checkers
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._
import org.scalacheck.Properties
import org.scalatest.{MustMatchers, WordSpec}

/**
  * Created by lkuligin on 06/07/2017.
  */
class Ch6PropSpec extends Properties("Ch6") with MustMatchers with TestHelper {
  val o = Ch6
  property("nonNegative") = forAll { (a: Long) =>
    val r = o.nonNegativeInt(SimpleRNG(a))
    r._1 >= 0 && o.nonNegativeInt(r._2)._1 >= 0
  }

  property("double") = forAll { (a: Long) =>
    val r1 = o.double(SimpleRNG(a))
    val r2 = o.double(r1._2)
    r1._1 != r2._1 && (Math.abs(r1._1 - r1._1.toInt) > 0 ||
      r1._1 == 0.0) && r1._1 >= 0 && r1._1 < 1 && r2._1 >= 0 && r2._1 < 1
  }

  property("double2") = forAll { (a: Long) =>
    val r = o.double2(SimpleRNG(a))
    r._1 >= 0 && r._1 < 1
  }

  property("unit state") = forAll { (seed: Long) =>
    val s: State[RNG, Int] = o.State[RNG, Int] (_.nextInt)
    s.unit(2).run(o.SimpleRNG(seed))._1 == 2
  }

  property("map state") = forAll { (seed: Long) =>
    val s: State[RNG, Int] = o.State[RNG, Int] (_.nextInt)
    s.map(_.toString()).run(o.SimpleRNG(seed))._1 == o.SimpleRNG(seed).nextInt._1.toString
  }

  property("flatMap state") = forAll { (seed: Long) =>
    val s: State[RNG, Int] = o.State[RNG, Int] (_.nextInt)
    val r = o.SimpleRNG(seed).nextInt
    s
      .flatMap(x => o.State[RNG, Int]( s1 => (s1.nextInt._1 + 2*x, s1.nextInt._2)))
      .run(o.SimpleRNG(seed))._1 == 2*r._1 + r._2.nextInt._1
  }

  property("map2 state") = forAll { (seed: Long) =>
    val s: State[RNG, Int] = o.State[RNG, Int] (_.nextInt)
    val s1: State[RNG, Double] = o.State[RNG, Int] (s => s.nextInt) map (_ * 0.5)
    s.map2(s1)((x,y) => x + y).run(o.SimpleRNG(seed))._1 == (
      o.SimpleRNG(seed).nextInt._1 + o.SimpleRNG(seed).nextInt._2.nextInt._1 * 0.5)
  }

  property("sequence2") = forAll { (seed: Long) =>
    val s: State[RNG, Int] = o.State[RNG, Int] (_.nextInt)
    val lst = List(s, s, s)

    val step1 = o.SimpleRNG(seed).nextInt
    val step2 = step1._2.nextInt
    val step3 = step2._2.nextInt

    o.sequence2(lst).run(o.SimpleRNG(seed))._1 == List(step3._1, step2._1, step1._1)
  }

}
