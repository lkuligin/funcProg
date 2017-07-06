package com.lkuligin.funprog

import com.lkuligin.funprog.Ch4._
import org.scalatest.{MustMatchers, WordSpec}

/**
  * Created by lkuligin on 28/06/2017.
  */
class Ch4Spec extends WordSpec with MustMatchers with TestHelper {

  val o = Ch4

  "MyOption" when {
    "map" in {
      MyNone.map(_ => true) mustBe MyNone
      MySome(1).map(_ +1 ) mustBe MySome(2)
    }
    "flatMap" in {
      MyNone.flatMap(_ => MyNone) mustBe MyNone
      MyNone.flatMap(_ => MySome(1)) mustBe MyNone
      MySome(1).flatMap(MySome(_)) mustBe MySome(1)
      MySome(1).flatMap(x => MySome(x+1)) mustBe MySome(2)
    }
    "gerOrElse" in {
      MyNone.getOrElse(1) mustBe 1
      MySome(1).getOrElse(2) mustBe 1
    }
    "orElse" in {
      MyNone.orElse(MySome(1)) mustBe MySome(1)
      MySome(1).orElse(MySome(2)) mustBe MySome(1)
    }
    "filter" in {
      MyNone.filter(_ =>true) mustBe MyNone
      MySome(1).filter(_ => true) mustBe MySome(1)
      MySome(1).filter(_ => false) mustBe MyNone
      MySome(2).filter(x => x % 2 == 0) mustBe MySome(2)
    }
  }

  "variance" should {
    "work properly" in {
      o.variance(List()) mustBe MyNone
      o.variance(List(1)) mustBe MySome(1.0)
      o.variance(List(1,2)) mustBe MySome(2.5)
    }
  }

  "map2" should {
    "work properly" in {
      o.map2(MyNone, MyNone)((_, _) => true) mustBe MyNone
      o.map2(MyNone, MySome(1))((_, _) => true) mustBe MyNone
      o.map2(MySome(1), MyNone)((_, _) => true) mustBe MyNone
      o.map2(MySome(1), MySome(2))((x, y) => x+y) mustBe MySome(3)
    }
  }

  "sequence" should {
    "work for Option" in {
      o.sequence(List(MyNone)) mustBe MyNone
      o.sequence(List(MySome(1), MyNone, MySome(2))) mustBe MyNone
      o.sequence(List(MySome(1), MySome(2))) mustBe MySome(List(1,2))
    }
    "work for Either" in {
      o.sequenceEither(List(MyLeft("err"))) mustBe MyLeft("err")
      o.sequenceEither(List(MyRight(1), MyLeft("err"), MyRight(2))) mustBe MyLeft("err")
      o.sequenceEither(List(MyRight(1), MyRight(2))) mustBe MyRight(List(1,2))
    }
  }

  "traverse" should {
    "work for Option" in {
      o.traverse(List())(x => MySome(true)) mustBe MySome(List())
      o.traverse(List("1", "a"))(x => {if (x.forall(_.isDigit)) MySome(x.toInt)
        else MyNone}) mustBe MyNone
      o.traverse(List("1", "2"))(x => {if (x.forall(_.isDigit)) MySome(x.toInt)
        else MyNone}) mustBe MySome(List(1,2))
    }
    "work for Either" in {
      o.traverse(List())(x => MySome(true)) mustBe MySome(List())
      o.traverse(List("1", "a"))(x => {if (x.forall(_.isDigit)) MySome(x.toInt)
      else MyNone}) mustBe MyNone
      o.traverse(List("1", "2"))(x => {if (x.forall(_.isDigit)) MySome(x.toInt)
      else MyNone}) mustBe MySome(List(1,2))
    }
  }

  "MyEither" should {
    "map" in {
      MyLeft("error").map(_ => true) mustBe MyLeft("error")
      MyRight(1).map(_ +1 ) mustBe MyRight(2)
    }
    "flatMap" in {
      MyLeft("error").flatMap(_ => MyLeft("another error")) mustBe MyLeft("error")
      MyLeft("error").flatMap(_ => MyRight(1)) mustBe MyLeft("error")
      MyRight(1).flatMap(MyRight(_)) mustBe MyRight(1)
      MyRight(1).flatMap(x => MyRight(x+1)) mustBe MyRight(2)
      MyRight(1).flatMap(x => MyLeft("error")) mustBe  MyLeft("error")
    }
    "orElse" in {
      MyLeft("err").orElse(MyRight(1)) mustBe MyRight(1)
      MyRight(1).orElse(MyRight(2)) mustBe MyRight(1)
    }
    "map2" in {
      MyRight(1).map2(MyRight(2))((x,y) => x+y) mustBe MyRight(3)
      MyRight("aaa").map2(MyRight("bb"))((x,y) => (x+y).length()) mustBe MyRight(5)
    }
  }




}
