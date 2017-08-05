package com.lkuligin.funprog

import com.lkuligin.funprog.Ch6.SimpleRNG
import com.lkuligin.funprog.Ch8.Prop2.{Falsified, Passed}
import com.lkuligin.funprog.Ch8.{Gen, Prop2}
import org.scalatest.{MustMatchers, WordSpec}

class Ch8Spec extends WordSpec with MustMatchers with TestHelper {
  val g = Gen
  val prop2 = Prop2
  val s = Ch6

  class TestFixture {
    val r = s.SimpleRNG(1000)
    val r1 = s.SimpleRNG(1002)
  }

  "Gen.choose" in new TestFixture {
    g.choose(10, 20).sample.run(r)._1 mustBe 14
  }

  "Gen.unit" in new TestFixture {
    g.unit(15).sample.run(r)._1 mustBe 15
  }

  "Gen.boolean" in new TestFixture {
    g.boolean.sample.run(r)._1 mustBe true
    g.boolean.sample.run(r1)._1 mustBe true
  }

  "Gen.listOfN" in new TestFixture {
    g.listOfN(3, g.choose(10, 20)).sample.run(r)._1 mustBe List(18,17,14)
  }

  "Gen.flatMap" in new TestFixture {
    val gen1 = g.choose(10, 20)
    gen1.flatMap(x => g.unit(x * 2)).sample.run(r)._1 mustBe 28
  }

  "Gen.listOfN2" in new TestFixture {
    val gen1 = g.choose(10, 20)
    gen1.listOfN(g.choose(10,20)).sample.run(r)._1 mustBe List(13, 15, 18, 15, 12, 14, 15, 12, 11, 11, 12, 17, 18, 17)
  }

  "Gen.union" in new TestFixture {
    g.union(g.choose(10,20), g.choose(20,30)).sample.run(r)._1 mustBe 17
  }

  "Gen.weighted" in new TestFixture {
    g.double.sample.run(r)._1 mustBe 0.8666026894313296
    //18 17 14 println(g.listOfN(3, g.choose(10,20)).sample.run(r))
    //28 27 14 println(g.listOfN(3, g.choose(20,30)).sample.run(r))
    g.weighted((g.choose(10,20), 0), (g.choose(20,30), 1)).sample.run(r)._1 mustBe 27
    g.weighted((g.choose(10,20), 1), (g.choose(20,30), 0)).sample.run(r)._1 mustBe 17
    g.weighted((g.choose(10,20), 6), (g.choose(20,30), 4)).sample.run(r)._1 mustBe 27
    g.weighted((g.choose(10,20), 9), (g.choose(20,30), 1)).sample.run(r)._1 mustBe 17
  }

  "Prop2" in new TestFixture {
    val gen1 = g.choose(10,20)
    val p1 = prop2.forAll(gen1)(_>10)
    p1.run(10, 10, r) mustBe Passed
  }

  "Prop2 && and ||" in new TestFixture {
    val gen1 = g.choose(10,20)
    val p1 = prop2.forAll(gen1)(_>10)
    val p2 = prop2.forAll(gen1)(_>20)
    val p3 = prop2.forAll(gen1)(_<20)
    p1.||(p2).run(10, 10, r) mustBe Passed
    p1.&&(p2).run(10, 10, r) mustBe Falsified("14",0)
    p1.&&(p3).run(10, 10, r) mustBe Passed
  }

  "SGen.listOf" in new TestFixture {
    val gen1 = g.unit(10)
    val p1 = prop2.forAll(g.listOf(gen1))(_.forall(_ == 10))
    p1.run(10, 10, r) mustBe Passed
  }

}
