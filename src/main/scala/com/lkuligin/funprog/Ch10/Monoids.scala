package com.lkuligin.funprog.Ch10

import language.higherKinds

object Monoids {

  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String): String = a1 + a2

    def zero: String = ""
  }

  /**
    * 10.1 Monoid instances for integer additions and multiplication as well as Boolean operators
    */
  val intAddition = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = a1 + a2

    def zero: Int = 0
  }

  val intMultiplication = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = a1 * a2

    def zero: Int = 1
  }

  val booleanAnd = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2

    def zero: Boolean = true
  }

  val booleanOr = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2

    def zero: Boolean = false
  }

  /**
    * 10.2 Monoid instance for combining two Option values
    */
  def optionMonoid[A] = new Monoid[Option[A]] {
    def op(a1: Option[A], a2: Option[A]): Option[A] = a1.orElse(a2)

    def zero: Option[A] = None
  }

  /**
    * 10.3 monoid for a function having the same argument and return type
    */
  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    def op(a1: A => A, a2: A => A): A => A = (a: A) => a2(a1(a))

    def zero: A => A = (a: A) => a
  }

  /**
    * 10.5 implement foldMap
    */
  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B = as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))

  /**
    * 10.6 write foldLeft and foldRight using foldMap
    */
  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = {
    val m = new Monoid[B => B] {
      def op(a1: B => B, a2: B => B): B => B = (b: B) => a1(a2(b))

      def zero: B => B = (a: B) => a
    }
    foldMap(as, m)(a => b => f(b, a))(z)
  }

  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = foldMap(as, endoMonoid[B])(f.curried)(z)

  /**
    * 10.7 implement foldMap for IndexedSeq
    */
  def foldMapV[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = v.length match {
    case 0 => m.zero
    case 1 => m.op(m.zero, f(v.head))
    case n => {
      val (left, right) = v.splitAt(n / 2)
      m.op(foldMapV(left, m)(f), foldMapV(right, m)(f))
    }
  }

  /**
    * 10.9 use foldMap to detect whether a given IndexedSeq[Int] is ordered
    */
  def isOrderedIndexedSeq(v: IndexedSeq[Int]): Boolean = {
    case class Observation(max: Int, min: Int, ordered: Boolean)

    val m = new Monoid[Option[Observation]] {
      override def op(a1: Option[Observation], a2: Option[Observation]) = (a1, a2) match {
        case (Some(Observation(max1, min1, o1)), Some(Observation(max2, min2, o2))) =>
          Some(Observation(Math.max(max1, max2), Math.min(min1, min2), o1 && o2 && max1 <= min2))
        case (Some(o), _) => Some(o)
        case (_, Some(o)) => Some(o)
        case (_, _) => None
      }

      def zero: Option[Observation] = None
    }

    foldMapV(v, m)(i => Some(Observation(i, i, true))).map(_.ordered).getOrElse(true)
  }

  sealed trait WC
  case class Stub(chars: String) extends WC
  case class Part(lStub: String, words: Int, rStub: String) extends WC

  /**
    * 10.10 monoid instance for WC
    */
  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    def op(a1: WC, a2: WC): WC = (a1, a2) match {
      case (Part(l1, w1, r1), Part(l2, w2, r2)) => Part(l1, w1+w2+(if (!(r1+l2).isEmpty) 1 else 0), r1+r2)
      case (Stub(s1), Stub(s2)) => Stub(s1+s2)
      case (Part(l, w, r), Stub(s)) => Part(l, w, r+s)
      case (Stub(s), Part(l, w, r)) => Part(s+l, w, r)
    }

    def zero: WC = Stub("")
  }

  /**
    * 10.11 implement a function that counts words in a String using wcMonoid
    */
  def count(a: String): Int = {
    def go(c: Char): WC = if (c.isWhitespace) Part("", 0, "") else Stub(c.toString)

    def words(w: String) = if (w.isEmpty) 0 else 1

    foldMapV(a.toIndexedSeq, wcMonoid)(go) match {
      case Stub(s) => words(s)
      case Part(l, w, r) => w + words(l) + words(r)
    }
  }

  /**
    * 10.16 implement product monoid
    */
  def productMonoid[A,B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] = new Monoid[(A,B)] {
    def op(a1: (A,B), a2: (A,B)) = (A.op(a1._1, a2._1), B.op(a1._2, a1._2))
    def zero = (A.zero, B.zero)
  }

}
