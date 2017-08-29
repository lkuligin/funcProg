package com.lkuligin.funprog

import org.scalatest.{MustMatchers, WordSpec}
import com.lkuligin.funprog.Ch12.ApplicativeImpl._

class Ch12Spec extends WordSpec with MustMatchers {

  "streamApplicative" in {
    streamApplicative.unit(1) mustBe Stream(1)
    streamApplicative.map2(Stream(1,2,3), Stream("b","bb","bbbb"))((x,y) => x+y.length) mustBe Stream(2,4,7)
  }

  "applicative extensions" should {
    "traverse" in {
      optionApplicative.traverse(List(Some("b"),Some("bb"),Some("bbbb")))(x => x.map(_.length+1)) mustBe Some(List(2,3,5))
    }
    "sequence" in {
      optionApplicative.sequence(List(Some(1), Some(2), Some(3))) mustBe Some(List(1,2,3))
    }
    "replicateM" in {
      optionApplicative.replicateM(2, Some(1)) mustBe Some(List(1, 1))
    }
    "product" in {
      optionApplicative.product(Some(1), Some("bb")) mustBe Some((1,"bb"))
    }
    "product of two functions" in {
      val o1 = streamApplicative
      val a = optionApplicative.product(o1)
      a.unit(2) mustBe (Some(2), Stream(2))
      a.map2((Some(1), Stream(2,4)), (Some("a"), Stream("b", "bb")))((a,b) => a+b.length) mustBe (Some(2), Stream(3,6))
    }
    "compose of two functions" in {
      val o1 = streamApplicative
      val a = optionApplicative.compose(o1)
      a.unit(2) mustBe Some(Stream(2))
      a.map2(Some(Stream(1,2)), Some(Stream("a", "bbb")))((a,b) => a + b.length) mustBe Some(Stream(2,5))
    }
    "sequenceMap" in {
      optionApplicative.sequenceMap(Map(1 -> Some("a"), 2 -> Some("bb"))) mustBe Some(Map(1 -> "a", 2 -> "bb"))
    }
  }
}
