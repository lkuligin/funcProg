package com.lkuligin.funprog.Ch8

import com.lkuligin.funprog.{Ch5, Ch6}
import com.lkuligin.funprog.Ch6.RNG
import com.lkuligin.funprog.Ch8.Prop2._

case class Prop2(run: (MaxSize, TestCases, RNG) => Result) {
  /**
    * 8.9 Implement && and || for composing Prop values
    */
  def &&(p: Prop2): Prop2 = Prop2((m, testCases, rng) => this.run(m, testCases, rng) match {
    case Passed => p.run(m, testCases, rng)
    case x => x
  })

  def ||(p: Prop2): Prop2 = Prop2((m, testCases, rng) => this.run(m, testCases, rng) match {
    case Falsified(_, _) => p.run(m, testCases, rng)
    case Passed => Passed
  })
}

object Prop2 {
  type SuccessCount = Int
  type TestCases = Int
  type MaxSize = Int
  type FailedCase = String

  sealed trait Result {
    def isFalsified: Boolean
  }

  case object Passed extends Result {
    def isFalsified = false
  }

  case class Falsified(failure: FailedCase,
                       successes: SuccessCount) extends Result {
    def isFalsified = true
  }

  case object Proved extends Result {
    def isFalsified = false
  }

  def apply(f: (TestCases,RNG) => Result): Prop2 = Prop2 { (_,n,rng) => f(n,rng) }

  def forAll[A](as: SGen[A])(f: A => Boolean): Prop2 = forAll(as.forSize(_))(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop2 = Prop2 {
    (max, n, rng) =>
      val casesPerSize = (n - 1) / max + 1
      val props: Stream[Prop2] = Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
      val prop: Prop2 =
        props.map(p => Prop2 { (max, n, rng) =>
          p.run(max, casesPerSize, rng)
        }).toList.reduce(_ && _)
      prop.run(max, n, rng)
  }

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop2 = Prop2 {
    (n, rng) =>
      randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
        case (a, i) => try {
          if (f(a)) Passed else Falsified(a.toString, i)
        } catch {
          case e: Exception => Falsified(buildMsg(a, e), i)
        }
      }.find(_.isFalsified).getOrElse(Passed)
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Ch5.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"""test case: $s\n generated an exception: ${e.getMessage}\n stack trace:\n ${e.getStackTrace.mkString("\n")}"""

  def run(p: Prop2,
          maxSize: Int = 100,
          testCases: Int = 100,
          rng: RNG = Ch6.SimpleRNG(System.currentTimeMillis)): Unit =
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) =>
        println(s"! Falsified after $n passed tests:\n $msg")
      case Passed =>
        println(s"+ OK, passed $testCases tests.")
      case Proved =>
        println(s"+ OK, proved property.")
    }
}