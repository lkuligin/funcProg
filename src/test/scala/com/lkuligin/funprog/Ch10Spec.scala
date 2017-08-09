package com.lkuligin.funprog

import com.lkuligin.funprog.Ch10._
import org.scalatest.{MustMatchers, WordSpec}

class Ch10Spec extends WordSpec with MustMatchers with TestHelper {

  val o = Monoids

  "foldMapV" in {
    val v: IndexedSeq[String] = IndexedSeq("test1", "aattza", "fsdf4r", "gdfgbtz")
    val m = o.intAddition
    o.foldMapV[String, Int](v, m)(s => s.length) mustBe 24
  }

  "isOrderedIndexedSeq" in {
    o.isOrderedIndexedSeq(IndexedSeq(1, 6, 100, 600, 601, 1000)) mustBe true
    o.isOrderedIndexedSeq(IndexedSeq(1)) mustBe true
    o.isOrderedIndexedSeq(IndexedSeq(100,-5, 600)) mustBe false
  }

  "count" in {
    o.count("bla bla bla bla") mustBe 4
    o.count("") mustBe 0
    o.count("fsdf fr tzui") mustBe 3
  }

  "foldable" should {
    "list" in {
      val fold = ListFoldable
      val l = List("aa","bbb","c")
      fold.foldLeft(l)(2)((a,b) => a*b.length) mustBe 12
      fold.foldRight(l)(2)((b,a) => a*b.length) mustBe 12
      fold.foldMap(l)(2)(_.length)(o.intMultiplication) mustBe 12
      fold.toList(l) mustBe l
    }
    "indexedSeq" in {
      val fold = IndexedSeqFoldable
      val l = IndexedSeq("aa","bbb","c")
      fold.foldLeft(l)(2)((a,b) => a*b.length) mustBe 12
      fold.foldRight(l)(2)((b,a) => a*b.length) mustBe 12
      fold.foldMap(l)(2)(_.length)(o.intMultiplication) mustBe 12
    }
    "stream" in {
      val fold = StreamFoldable
      val l = Stream("aa","bbb","c")
      fold.foldLeft(l)(2)((a,b) => a*b.length) mustBe 12
      fold.foldRight(l)(2)((b,a) => a*b.length) mustBe 12
      fold.foldMap(l)(2)(_.length)(o.intMultiplication) mustBe 12
    }
    "tree" in {
      val fold = TreeFoldable
      val l: Ch10.Tree[String] = Ch10.Branch(Ch10.Branch(Ch10.Leaf("aa"), Ch10.Leaf("bbb")), Ch10.Leaf("c"))
      fold.foldLeft(l)(2)((a,b) => a*b.length) mustBe 12
      fold.foldRight(l)(2)((b,a) => a*b.length) mustBe 12
      fold.foldMap(l)(2)(_.length)(o.intMultiplication) mustBe 12
    }
    "option" in {
      val fold = OptionFoldable
      val l = Some("aa")
      fold.foldLeft(l)(2)((a,b) => a*b.length) mustBe 4
      fold.foldRight(l)(2)((b,a) => a*b.length) mustBe 4
      fold.foldMap(l)(2)(_.length)(o.intMultiplication) mustBe 4
    }
  }
}
