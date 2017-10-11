package com.lkuligin.cats.monads

import org.scalatest.{MustMatchers, WordSpec}
import MonadExcercises._
import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import cats.instances.list._

class MonadExcercisesTest extends  WordSpec with MustMatchers {

  "factorial" in {
    val res: Seq[MonadExcercises.Logged[Int]] = Await.result(Future.sequence(Vector(
      Future(factorial(3)),
      Future(factorial(2))
    )), 5.seconds)
    res(0).run._1 mustBe Vector("fact 0 1", "fact 1 1", "fact 2 2", "fact 3 6")
    res(1).run._1 mustBe Vector("fact 0 1", "fact 1 1", "fact 2 2")
    res.map(x => x.run._2) mustBe Vector(6, 2)
  }

  "product" in {
    product(List(1, 2), List(3, 4, 5)) mustBe List((1,3), (1,4), (1,5), (2,3), (2,4), (2,5))
  }
}
