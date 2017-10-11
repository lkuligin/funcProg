package com.lkuligin.cats.monads

import cats.Cartesian
import cats.data.Validated
import cats.syntax.either._
import cats.instances.list._

import scala.language.higherKinds

object FormValidation {
  case class User(name: String, age: Int)

  type FormData = Map[String, String]
  type ErrorsOr[A] = Either[List[String], A]
  type AllErrorsOr[A] = Validated[List[String], A]

  def getValue(name: String)(data: FormData): ErrorsOr[String] = data.get(name).toRight(List(s"field $name is not entered"))

  def parseInt(name: String)(data: String): ErrorsOr[Int] =
    Right(data).flatMap(s => Either.catchOnly[NumberFormatException](s.toInt)).leftMap(_ => List(s"$name must be an integer"))

  def nonBlank(name: String)(data: String): ErrorsOr[String] =
    Right(data).asInstanceOf[Either[List[String], String]].ensure(List(s"$name cannot be blank"))(_.nonEmpty)

  def nonNegative(name: String)(data: Int): ErrorsOr[Int] =
    Right(data).asInstanceOf[Either[List[String], Int]].ensure[List[String]](List(s"$name must be nonnegative"))(_ >= 0)

  def readName(data: FormData): ErrorsOr[String] = getValue("name")(data).flatMap(nonBlank("name"))

  def readAge(data: FormData): ErrorsOr[Int] = {
    val field = "age"
    getValue(field)(data).
      flatMap(nonBlank(field))
      .flatMap(parseInt(field))
      .flatMap(nonNegative(field))
  }

  def readUser(data: FormData): AllErrorsOr[User] = Cartesian[AllErrorsOr].product(readName(data).toValidated, readAge(data).toValidated).map(User.tupled)
}
