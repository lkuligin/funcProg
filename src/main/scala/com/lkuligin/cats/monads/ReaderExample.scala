package com.lkuligin.cats.monads

import cats.data.Reader
import cats.syntax.applicative._

object ReaderExample {
  case class Db(usernames: Map[Int, String], passwords: Map[String, String])

  type DbReader[A] = Reader[Db, A]

  def findUsername(userId: Int): DbReader[Option[String]] = Reader(x => x.usernames.get(userId))

  def checkPassword(username: String, password: String): DbReader[Boolean] = Reader(x => (x.passwords.get(username).contains(password)))

  def checkLogin(userId: Int, password: String): DbReader[Boolean] = {
    for {
      username <- findUsername(userId)
      passwordValid <- username.map(x => checkPassword(x, password)).getOrElse(false.pure[DbReader])
    } yield passwordValid
  }

}
