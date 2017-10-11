package com.lkuligin.cats.cases

import cats.Monoid
import cats.instances.future._
import cats.instances.vector._
import cats.syntax.foldable._
import cats.syntax.monoid._
import cats.syntax.traverse._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._


object PygmyHadoop {
  def foldMap[A, B: Monoid](values: Vector[A])(func: A => B): B =
    values.map(func).foldLeft(Monoid[B].empty)(_ |+| _)

  def parallelFoldMap[A, B: Monoid](values: Vector[A])(func: A => B): Future[B] = {
    val cpuAvailable = Runtime.getRuntime.availableProcessors()
    val size = (1.0 * values.size / cpuAvailable).ceil.toInt

    val groups: Iterator[Vector[A]] = values.grouped(size)

    val futures: Iterator[Future[B]] = groups.map(x => Future(foldMap(x)(func)))

    Future.sequence(futures).map(x => x.foldLeft(Monoid[B].empty)(_ |+| _))

  }

  def parallelFoldMap2[A, B: Monoid](values: Vector[A])(func: A => B): Future[B] = {
    val cpuAvailable = Runtime.getRuntime.availableProcessors()
    val size = (1.0 * values.size / cpuAvailable).ceil.toInt

    values
      .grouped(size)
      .toVector
      .traverse(group => Future(group.toVector.foldMap(func)))
      .map(_.combineAll)
  }


}
