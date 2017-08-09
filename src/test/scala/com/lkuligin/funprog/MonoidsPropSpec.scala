package com.lkuligin.funprog

import com.lkuligin.funprog.Ch10.Monoids
import org.scalacheck.Properties
import org.scalatest.MustMatchers
import org.scalacheck.Prop._

class MonoidsPropSpec extends Properties("Monoids") with MustMatchers with TestHelper {
  val o = Monoids

  /**
    * 10.4 implement properties for monoid laws
    */

  property("associativeIntAddition") = forAll { (a1: Int, a2: Int, a3: Int) =>
    val m = o.intAddition
    m.op(m.op(a1,a2), a3) == m.op(a1, m.op(a2, a3))
  }

  property("identityIntAddition") = forAll { (a1: Int) =>
    val m = o.intAddition
    m.op(a1, m.zero) == m.op(m.zero, a1)
  }

  property("associativeIntMultiplication") = forAll { (a1: Int, a2: Int, a3: Int) =>
    val m = o.intMultiplication
    m.op(m.op(a1,a2), a3) == m.op(a1, m.op(a2, a3))
  }

  property("identityIntMultiplication") = forAll { (a1: Int) =>
    val m = o.intMultiplication
    m.op(a1, m.zero) == m.op(m.zero, a1)
  }

  property("associativeBooleanAnd") = forAll { (a1: Boolean, a2: Boolean, a3: Boolean) =>
    val m = o.booleanAnd
    m.op(m.op(a1,a2), a3) == m.op(a1, m.op(a2, a3))
  }

  property("identityBooleanAnd") = forAll { (a1: Boolean) =>
    val m = o.booleanAnd
    m.op(a1, m.zero) == m.op(m.zero, a1)
  }

  property("associativeBooleanOr") = forAll { (a1: Boolean, a2: Boolean, a3: Boolean) =>
    val m = o.booleanOr
    m.op(m.op(a1,a2), a3) == m.op(a1, m.op(a2, a3))
  }

  property("identityBooleanOr") = forAll { (a1: Boolean) =>
    val m = o.booleanOr
    m.op(a1, m.zero) == m.op(m.zero, a1)
  }

  property("associativeOptionMonoid") = forAll { (a1: String, a2: String, a3: String) =>
    val m = o.optionMonoid[String]
    m.op(m.op(Some(a1),Some(a2)), Some(a3)) == m.op(Some(a1), m.op(Some(a2), Some(a3)))
    m.op(m.op(None,Some(a2)), Some(a3)) == m.op(None, m.op(Some(a2), Some(a3)))
    m.op(m.op(Some(a1),None), Some(a3)) == m.op(Some(a1), m.op(None, Some(a3)))
    m.op(m.op(Some(a1),Some(a2)), None) == m.op(Some(a1), m.op(Some(a2), None))
  }

  property("identityOptionMonoid") = forAll { (a1: String) =>
    val m = o.optionMonoid[String]
    m.op(Some(a1), m.zero) == m.op(m.zero, Some(a1))
    m.op(None, m.zero) == m.op(m.zero, None)
  }

  property("endoMonoid") = forAll { (a: String) =>
    val m = o.endoMonoid[String]
    def f1(a: String) = a.toLowerCase
    def f2(a: String) = a.toUpperCase
    def f3(a: String) = a.replace("a", "b")
    def f4(a: String) = a.replace("b", "c")
    m.op(m.op(f1,f2), f3)(a) == m.op(f1, m.op(f2, f3))(a)
    m.op(m.op(f1,f3), f4)(a) == m.op(f1, m.op(f3, f4))(a)
    m.op(f1, m.zero)(a) == m.op(m.zero, f1)(a)
    m.op(f2, m.zero)(a) == m.op(m.zero, f2)(a)
    m.op(f3, m.zero)(a) == m.op(m.zero, f3)(a)
  }

  property("foldMap") = forAll { (a: Int) =>
    val m = o.intAddition
    val l: List[Int] = List(a, a+1, a+2)
    o.foldMap[Int, Int](l, m)(x => x*2) == 6*a+6
  }

}
