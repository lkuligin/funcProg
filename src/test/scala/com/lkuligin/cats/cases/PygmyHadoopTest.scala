package com.lkuligin.cats.cases

import org.scalatest.{MustMatchers, WordSpec}
import PygmyHadoop._
import cats.instances.int._
import cats.instances.string._

import scala.concurrent.Await
import scala.concurrent.duration._


class PygmyHadoopTest extends WordSpec with MustMatchers {
  "foldMap" in {
    foldMap(Vector(1, 2, 3))(_.toString + "! ") mustBe "1! 2! 3! "
    foldMap("Hello world!".toVector)(_.toString.toUpperCase) mustBe "HELLO WORLD!"
  }

  "parallel foldMap" in {
    Await.result(parallelFoldMap((1 to 1000000).toVector)(identity), 1 second) mustBe 1784293664
    Await.result(parallelFoldMap2((1 to 1000000).toVector)(identity), 1 second) mustBe 1784293664
  }
}
