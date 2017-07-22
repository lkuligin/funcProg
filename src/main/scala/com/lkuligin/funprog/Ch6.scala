package com.lkuligin.funprog

/**
  * Created by lkuligin on 06/07/2017.
  */
object Ch6 {
  trait RNG {
    def nextInt: (Int, RNG)
  }

  case class SimpleRNG(seed: Long) extends  RNG {
    override def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE666DL + 0xBL) & 0xFFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def unit[A](a: A): Rand[A] = rng => (a, rng)


  /**
    * 6.1 generates a random integer between 0 and Int.maxValue
    */
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val r = rng.nextInt
    (Math.abs(r._1), r._2)
  }

  /**
    * 6.2 generates a Double between 0 and 1, [0,1)
    */
  def double(rng: RNG): (Double, RNG) = {
    val r1 = nonNegativeInt(rng)
    (1.0*r1._1/Int.MaxValue, r1._2)
  }

  /**
    * 6.3 generates (Int, Double), (Double, Int) and (Double, Double, Double) random pairs
    */
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r1) = nonNegativeInt(rng)
    val (d, r2) = double(r1)
    ((i,d), r2)
  }
  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (p, r) = intDouble(rng)
    ((p._2, p._1), r)
  }
  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1,d2, d3), r3)
  }

  /**
    * 6.4 generates a list of random integers
    */
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @annotation.tailrec
    def go(n: Int, acc: List[Int], r: RNG): (List[Int], RNG) =
      if (n < 1) (acc, r) else
        {
          val (i, r1) = r.nextInt
          go(n-1, i :: acc, r1)
        }
    go(count, List(), rng)
  }

  /**
    * 6.5 implement double with map
    */
  def double2: Rand[Double] = map(nonNegativeInt)(1.0*_/Int.MaxValue)

  def test = {
    val r = SimpleRNG(56)
    val o: Rand[Int] = _.nextInt
    o(r)._1
    val b: Rand[Int] = map(o)(x => x)

  }

  /**
    * 6.6 implement map2
    */
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A,B) => C): Rand[C] = rng => {
    val r1 = ra(rng)
    val r2 = rb(r1._2)
    (f(r1._1, r2._1), r2._2)
  }

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] = map2(ra, rb)((_, _))

  /**
    * 6.7 combines a List of transitions into a single transition
    */
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = rng => fs.foldLeft((List(): List[A], rng))((el, f) => {
      val r1: (A, RNG) = f(el._2)
      (r1._1 :: el._1, r1._2)
    })

  /**
    * 6.8 implement flatMap and nonNegativeLessThan using flatMap
    */

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (el, rng1) = f(rng)
    g(el)(rng1)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = flatMap(nonNegativeInt)(x => unit(x % n))

  /**
    * 6.9 implement map and map2 with flatMap
    */
  def mapA[A,B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(x => unit(f(x)))
  //map(rng => a)(f: a=>b) = flatMap(rng => a)(a => (f(a), rng))

  def map2A[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A,B) => C): Rand[C] = flatMap(ra)(x => map(rb)(f(x,_)))

  /**
    * 6.10 generalize unit, map, map2, flatMap, sequence for State
    */
  case class State[S, A](run: S => (A,S)) {
    def unit[B](v: B): State[S, B] = State(s => (v, s))

    def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
      val (a, s1) = this.run(s)
      f(a).run(s1)
    })

    def get[S]: State[S, S] = State(s => (s, s))

    def set[S](s: S): State[S, Unit] = State(_ => ((), s))

    def map[B](f: A => B): State[S, B] = flatMap(x => unit(f(x)))

    def map2[B,C](sb: State[S, B])(f: (A,B) => C): State[S, C] = this.flatMap(x => sb.map(f(x,_)))
  }

  def sequence2[S, A](fs: List[State[S, A]]): State[S, List[A]] = State (
    s => {
      fs.foldLeft((List[A](), s))((p, s1) => {
        val v = s1.run(p._2)
        (v._1 :: p._1, v._2)
      })
    }
  )

  /**
    * 6.11 returns number of coins and candies left in the machine
    *  insert a coin => unlock, turn & unlock => dispense and lock, turn & lock or coin & unlock => nothing, no candy => ignore
    */
  sealed trait Input
  case object Coin extends Input
  case object Turn extends Input

  case class Machine(locked: Boolean, candies: Int, coins: Int) {
    def proceed(input: Input): Machine = input match {
      case _ if candies <= 0 => Machine(locked, candies, coins)
      case Coin if locked => Machine(false, candies, coins + 1)
      case Turn if !locked => Machine(true, candies-1, coins)
      case _ => Machine(locked, candies, coins)
    }
  }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    State(s => {
      val s1 = inputs.foldLeft(s)((s, i) => s.proceed(i))
      ((s1.candies, s1.coins), s1)
    })

}
