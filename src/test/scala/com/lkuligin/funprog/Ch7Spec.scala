package com.lkuligin.funprog

import java.util.concurrent.Executors

import com.lkuligin.funprog.Ch7.Par
import com.lkuligin.funprog.Ch7.Par.Par
import org.scalatest.{MustMatchers, WordSpec}

class Ch7Spec extends WordSpec with MustMatchers with TestHelper {
  val p = Par

  val es = Executors.newFixedThreadPool(2)

  "map2t" in {
    val p2: Par[Int] = p.map2t[Int, String, Int](Par.unit(1), Par.unit("two"))(_ + _.length)
    p.run(es)(p2).get mustBe 4
  }

  "asyncF" in {
    val f1 = p.asyncF((x: Int) => x + 1)
    p.run(es)(f1(1)).get mustBe 2

    val f2 = p.asyncF((x: Int) => x.toString)
    p.run(es)(f2(100)).get mustBe "100"
  }

  "sequence" in {
    val f1: List[Par[Int]] = List(1, 2, 3).map(p.unit)
    p.run(es)(p.sequence(f1)).get mustBe List(1, 2, 3)
  }

  "parFilter" in {
    val f: Par[List[Int]] = p.parFilter(List.range(1, 6))(_ > 2)
    p.run(es)(f).get mustBe List(3, 4, 5)
  }

  "choices" should {
    "work properly for choiceN" in {
      val f: Par[Int] = p.choiceN(p.unit(2))(List.range(1, 6).map(p.unit))
      p.run(es)(f).get mustBe 3
    }
    "work properly for choice" in {
      p.run(es)(p.choice(p.unit(true))(p.unit(10), p.unit(20))).get mustBe 10
      p.run(es)(p.choice(p.unit(false))(p.unit(10), p.unit(20))).get mustBe 20
    }
    "work properly for choiceN2" in {
      val m: Map[Int, Par[Int]] = List.range(1, 6).map(x => x -> p.unit(x + 1)).toMap
      val f: Par[Int] = p.choiceN2(p.unit(2))(m)
      p.run(es)(f).get mustBe 3
    }
    "work properly for chooser" in {
      val f = p.chooser(p.unit(2))((x: Int) => p.unit(x * 2))
      p.run(es)(f).get mustBe 4
    }
  }

  "join" in {
    val f: Par[Par[String]] = p.unit(p.unit("bla-bla"))
    p.run(es)(p.join(f)).get mustBe "bla-bla"
  }
}
