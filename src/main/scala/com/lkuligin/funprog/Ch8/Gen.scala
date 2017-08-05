package com.lkuligin.funprog.Ch8

import com.lkuligin.funprog.Ch6
import com.lkuligin.funprog.Ch6.{RNG, State, nonNegativeInt}
import com.lkuligin.funprog.Ch8.SGen

case class Gen[A] (sample: State[RNG, A]) {
  /**
    * 8.6 Implement flatMap and listOfN
    */
  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(sample.flatMap(f(_).sample))
  def listOfN(size: Gen[Int]): Gen[List[A]] = size.flatMap(Gen.listOfN(_, this))

  /**
    * 8.10 converts Gen to SGen
    */
  def unsized: SGen[A] = SGen(_ => this)
}

object Gen {

  /**
    * 8.4 generate integerst in the given range
    */
  def choose(start: Int, stopExclusive: Int): Gen[Int] = Gen(State(nonNegativeInt(_)).map(start + _ % (stopExclusive-start)))

  /**
    * 8.5
    */
  def unit[A](a: => A): Gen[A] = Gen[A](State[RNG, A](Ch6.unit(a)))
  def boolean: Gen[Boolean] = Gen(State(nonNegativeInt(_)).map(_ % 2 == 0))
  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = {
    Gen(Ch6.sequence2(List.fill(n)(g.sample)))
  }

  /**
    * 8.7 combines 2 generators of the same type into one
    */
  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = boolean.flatMap(if (_) g1 else g2)

  /**
    * 8.8
    */
  def double: Gen[Double] = Gen(State(Ch6.double(_)))
  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = double.flatMap(x => if (x < (g1._2.abs / (g1._2.abs + g2._2.abs))) g1._1 else g2._1)

  /**
    * 8.12 combinator that doesn't accept an explicit size
    */
  def listOf[A](g: Gen[A]): SGen[List[A]] = SGen(listOfN(_, g))

  /**
    * 8.13 generates nonempty lists
    */
  def listOf1[A](g: Gen[A]): SGen[List[A]] = SGen(n => listOfN(n max 1, g))
}
