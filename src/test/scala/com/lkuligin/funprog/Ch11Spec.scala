package com.lkuligin.funprog

import org.scalatest.{MustMatchers, WordSpec}
import com.lkuligin.funprog.Ch11.MonadsImpl._

class Ch11Spec extends WordSpec with MustMatchers {

  "monad implementation" should {
    "work for options" in {
      monadOption.unit(1) mustBe Some(1)
      monadOption.map(Some("bla-bla"))(_.length) mustBe Some(7)
      monadOption.flatMap(Some("bla-bla"))(x => if (x.length < 3) Some(x.length) else None) mustBe None
      monadOption.flatMap(Some("bla-bla"))(x => if (x.length > 3) Some(x.length) else None) mustBe Some(7)
    }

    "work for Lists" in {
      monadList.unit(1) mustBe List(1)
      monadList.map(List("bla","blabla","a"))(_.length) mustBe List(3,6,1)
      monadList.flatMap(List(1,2,3))(x => List.range(0,x)) mustBe List(0,0,1,0,1,2)
    }

    "work for Streams" in {
      monadStream.unit(1) mustBe Stream(1)
      monadStream.map(Stream("bla","blabla","a"))(_.length) mustBe Stream(3,6,1)
      monadStream.flatMap(Stream(1,2,3))(x => Stream.range(0,x)) mustBe Stream(0,0,1,0,1,2)
    }
  }

  "monad sequence" in {
    monadOption.sequence(List(Some(1), Some(2), Some(3))) mustBe Some(List(1,2,3))
  }

  "monad traverse" in {
    monadOption.traverse(List(Some(1), Some(2), Some(3)))(x => x.map(_+1)) mustBe Some(List(2,3,4))
  }

  "monad replicateM" in {
    monadOption.replicateM(2, Some(1)) mustBe Some(List(1, 1))
  }

  "filterM" in {
    monadOption.filterM(List(Some(1), Some(2), Some(3)))(x => x.map(_ > 1)) mustBe Some(List(Some(2), Some(3)))
  }

  "compose" in {
    monadOption.compose[Option[Int], List[Int], List[Int]](x => Option(List(x, x.map(_+1)).flatten), y => Some(y.map(3 + _)))(Some(1)) mustBe Some(List(4,5))
  }

  "join" in {
    monadOption.join(Some(Some(1))) mustBe Some(1)
  }

}
