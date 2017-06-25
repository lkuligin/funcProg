package com.lkuligin.funprog

import com.lkuligin.funprog.Ch3._
import org.scalatest.{MustMatchers, WordSpec}
/**
  * Created by lkuligin on 22/06/2017.
  */
class Ch3Spec extends WordSpec with MustMatchers with TestHelper  {

  val ch3 = Ch3

  "list class" when {
    "tailed with 1" should {
      "run properly" in {
        ch3.tail(MyList(1,2,3)) mustBe MyList(2,3)
        ch3.tail(MyList[Int]()) mustBe MyNil
        ch3.tail(MyList("a", "b", "c")) mustBe MyList("b", "c")
        ch3.tail(MyList("a")) mustBe MyNil
      }
    }
    "tailed with n" should {
      "run properly" in {
        ch3.tail(1, MyList(1,2,3)) mustBe MyList(2,3)
        ch3.tail(5, MyList[Int]()) mustBe MyNil
        ch3.tail(2, MyList(1,2,3)) mustBe MyList(3)
        ch3.tail(3, MyList(1,2,3)) mustBe MyNil
        ch3.tail(10, MyList("a", "b", "c")) mustBe MyNil
        ch3.tail(1, MyList("a")) mustBe MyNil
      }
    }

    "setHead" should {
      "run properly" in {
        ch3.setHead(5, MyList(1,2,3)) mustBe MyList(5, 2,3)
        ch3.setHead(1, MyList[Int]()) mustBe MyList(1)
        ch3.setHead("d", MyList("a", "b", "c")) mustBe MyList("d", "b", "c")
      }
    }

    "dropWhile" should {
      "run properly" in {
        ch3.dropWhile(MyList(1,2,3), (x:Int) => true) mustBe MyNil
        ch3.dropWhile(MyList(1,2,3), (x:Int) => false) mustBe MyList(1,2,3)
        ch3.dropWhile(MyNil, (x:Int) => false) mustBe MyNil
        ch3.dropWhile(MyList(2,4,6, 7, 8), (x:Int) => x % 2 == 0) mustBe MyList(7,8)
      }
    }

    "init" should {
      "run properly" in {
        ch3.init(MyNil) mustBe MyNil
        ch3.init(MyList(1)) mustBe MyNil
        ch3.init(MyList(1,2,3)) mustBe MyList(1,2)
      }
    }

    "length" should {
      "run properly" in {
        ch3.length(MyNil) mustBe 0
        ch3.length(MyList(1)) mustBe 1
        ch3.length(MyList(1,2,3)) mustBe 3
      }
    }

    "sum" should {
      "run properly" in {
        ch3.sum(MyList(1,2,3)) mustBe 6
        ch3.sum(MyList(1)) mustBe 1
        ch3.sum(MyList()) mustBe 0
      }
    }

    "reverse" should {
      "run properly" in {
        ch3.reverse(MyList()) mustBe MyNil
        ch3.reverse(MyList(1)) mustBe MyList(1)
        ch3.reverse(MyList(1,2,3)) mustBe MyList(3,2,1)
      }
    }

    "foldLeft in terms of foldRight" should {
      "run properly" in {
        ch3.foldLeftA(MyList(), false)((x: Boolean, y: Int) => true) mustBe false
        ch3.foldLeftA(MyList(), 1)((x: Int, y: Int) => x+y) mustBe 1
        ch3.foldLeftA(MyList(1,2,3), 0)((x: Int, y: Int) => x+y) mustBe 6
        ch3.foldLeftA(MyList("a","bc","d"), 1)((x: Int, y: String) => x + y.length)  mustBe 5
      }
    }

    "foldRight in terms of folLeft" should {
      "run properly" in {
        ch3.foldRightA(MyList(), false)((x: Int, y: Boolean) => true) mustBe false
        ch3.foldRightA(MyList(), 1)((x: Int, y: Int) => x+y) mustBe 1
        ch3.foldRightA(MyList(1,2,3), 0)((x: Int, y: Int) => x+y) mustBe 6
        ch3.foldRightA(MyList("a","bc","d"), 1)((x: String, y: Int) => y + x.length)  mustBe 5
      }
    }

    "append" should {
      "deal properly with empty Lists" in {
        ch3.append(MyList(), MyList()) mustBe MyNil
        ch3.append(MyList(1,2), MyList()) mustBe MyList(1,2)
        ch3.append(MyList(), MyList(1,2)) mustBe MyList(1,2)
      }
      "append list to a list" in {
        ch3.append(MyList(1,2,3), MyList(1,3,4)) mustBe MyList(1,2,3,1,3,4)
        ch3.append(MyList("a","b"), MyList("c")) mustBe MyList("a","b","c")
      }
    }

    "concat" should {
      "run properly" in {
        ch3.concat(MyList(1,2,3), MyList(4), MyList(), MyList(5,6), MyList()) mustBe MyList(1,2,3,4,5,6)
        ch3.concat(MyList(1,2)) mustBe MyList(1,2)
        ch3.concat(MyList()) mustBe MyNil
      }
      "same for foldLeft implementation" in {
        ch3.concatA(MyList(1,2,3), MyList(4), MyList(), MyList(5,6), MyList()) mustBe MyList(1,2,3,4,5,6)
        ch3.concatA(MyList(1,2)) mustBe MyList(1,2)
        ch3.concatA(MyList()) mustBe MyNil
      }
    }

    "adds1" should {
      "run properly" in {
        ch3.adds1(MyList()) mustBe MyNil
        ch3.adds1(MyList(-1, 0, 5)) mustBe MyList(0, 1, 6)
        ch3.adds1(MyList(1)) mustBe MyList(2)
      }
    }

    "doubleToString" should {
      "run properly" in {
        ch3.doubleToString(MyList()) mustBe MyNil
        ch3.doubleToString(MyList(-1.3, 0.4, 5.5)) mustBe MyList("-1.3", "0.4", "5.5")
        ch3.doubleToString(MyList(1.456)) mustBe MyList("1.456")
      }
    }

    "map" should {
      "run properly" in {
        ch3.map[Int, Boolean](MyList())(_ => true) mustBe MyNil
        ch3.map[Int, Int](MyList(1,2,3))(_ + 1) mustBe MyList(2,3,4)
      }
    }

    "filter" should {
      "run properly" in {
        ch3.filter(MyList())(_ => true) mustBe MyNil
        ch3.filter(MyList(1,2,3))(_ => true) mustBe MyList(1,2,3)
        ch3.filter(MyList(1,2,3))(_ => false) mustBe MyNil
        ch3.filter(MyList(1,2,3,4,5))(_ % 2 == 0) mustBe MyList(2,4)
        ch3.filter(MyList(1,2,3,4,5))(_ > 6) mustBe MyNil
      }
      "same for the implementation via flatMap" in {
        ch3.filterA(MyList())(_ => true) mustBe MyNil
        ch3.filterA(MyList(1,2,3))(_ => true) mustBe MyList(1,2,3)
        ch3.filterA(MyList(1,2,3))(_ => false) mustBe MyNil
        ch3.filterA(MyList(1,2,3,4,5))(_ % 2 == 0) mustBe MyList(2,4)
        ch3.filterA(MyList(1,2,3,4,5))(_ > 6) mustBe MyNil
      }
    }

    "flatMap" should {
      "run properly" in {
        ch3.flatMap[Int, Int](MyList(1,2,3))(_ => MyNil) mustBe MyNil
        ch3.flatMap[Int, Int](MyList(1,2,3))(x => MyList(x, x+1)) mustBe MyList(1,2,2,3,3,4)
      }
    }

    "zip" should {
      "throw exeption for unequal lists" in {
        assertThrows[IllegalArgumentException]{ ch3.zip(MyList(1,2), MyList(2,3,4))}
        assertThrows[IllegalArgumentException]{ ch3.zip(MyList(1,2), MyList())}
      }
      "run properly" in {
        ch3.zip(MyList(), MyList()) mustBe MyNil
        ch3.zip(MyList(1,2,3), MyList(3,4,5)) mustBe MyList(4,6,8)
      }
    }

    "zipWith" should {
      "throw exeption for unequal lists" in {
        assertThrows[IllegalArgumentException]{ ch3.zipWith[Int, Int](MyList(1,2), MyList(2,3,4))(_ + _)}
        assertThrows[IllegalArgumentException]{ ch3.zipWith[Int, Int](MyList(1,2), MyList())(_ + _)}
      }
      "run properly" in {
        ch3.zipWith[Int, Boolean](MyList(), MyList())((_,_) => true) mustBe MyNil
        ch3.zipWith[Int, Int](MyList(1,2,3), MyList(3,4,5))(_ + _) mustBe MyList(4,6,8)
        ch3.zipWith[Int, Boolean](MyList(1,2,3), MyList(3,2,5))(_ == _) mustBe MyList(false, true, false)
      }
    }

    "hasSubSequence" should {
      "run properly" in {
        ch3.hasSubSequence(List(1,2,3,4), List(2,3,4)) mustBe true
        ch3.hasSubSequence(List(1,2,3,4), List(2,3)) mustBe true
        ch3.hasSubSequence(List(1,2,3,4), List(2,3,5)) mustBe false
        ch3.hasSubSequence(List(), List(2,3)) mustBe false
        ch3.hasSubSequence(List(), List()) mustBe true
        ch3.hasSubSequence(List(2,3), List()) mustBe true
      }
    }

    "size" should {
      val l = Leaf(1)
      "run properly" in {
        ch3.size(l) mustBe 1
        ch3.size(Branch(l, l)) mustBe 3
        ch3.size(Branch(Branch(l, l), Branch(l, l))) mustBe 7
      }
      "same for size implemented with fold" in {
        ch3.sizeA(l) mustBe 1
        ch3.sizeA(Branch(l, l)) mustBe 3
        ch3.sizeA(Branch(Branch(l, l), Branch(l, l))) mustBe 7
      }
    }

    "maxEl" should {
      val l = Leaf(1)
      "run properly" in {
        ch3.maxEl(Leaf(1)) mustBe 1
        ch3.maxEl(Branch(Branch(l, l), Branch(l, l))) mustBe 1
        ch3.maxEl(Branch(Branch(l, Leaf(3)), Branch(l, Leaf(4)))) mustBe 4
      }
      "same for maxEl implemented with fold" in {
        ch3.maxElA(Leaf(1)) mustBe 1
        ch3.maxElA(Branch(Branch(l, l), Branch(l, l))) mustBe 1
        ch3.maxElA(Branch(Branch(l, Leaf(3)), Branch(l, Leaf(4)))) mustBe 4
      }
    }

    "depth" should {
      val l = Leaf(1)
      "run properly" in {
        ch3.depth(l) mustBe 1
        ch3.depth(Branch(Branch(Branch(l,l),l),l)) mustBe 4
      }
      "same if implemented with fold" in {
        ch3.depthA(l) mustBe 1
        ch3.depthA(Branch(Branch(Branch(l,l),l),l)) mustBe 4
      }
    }

    "map tree" should {
      val l = Leaf(1)
      "run properly" in {
        ch3.map(l)(_+1) mustBe Leaf(2)
        ch3.map(Branch(Branch(Branch(Leaf(1),Leaf(2)),Leaf(3)),Leaf(1)))(_*2) mustBe Branch(Branch(Branch(Leaf(2),Leaf(4)),Leaf(6)),Leaf(2))
      }
      "same if implemented with fold" in {
        ch3.mapA(l)(_+1) mustBe Leaf(2)
        ch3.mapA(Branch(Branch(Branch(Leaf(1),Leaf(2)),Leaf(3)),Leaf(1)))(_*2) mustBe Branch(Branch(Branch(Leaf(2),Leaf(4)),Leaf(6)),Leaf(2))
      }
    }

  }
}
