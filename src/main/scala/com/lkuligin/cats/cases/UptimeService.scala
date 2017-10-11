package com.lkuligin.cats.cases

import cats.Applicative
import cats.instances.list._
import cats.syntax.functor._
import cats.syntax.traverse._

import scala.language.higherKinds

class UptimeService[F[_]](client: UptimeClient[F])(implicit a: Applicative[F]) {
  def getTotalUptime(hostNames: List[String]): F[Int] = {
    hostNames.traverse(client.getUptime).map(_.sum)
  }
}