package com.lkuligin.cats.monads

import org.scalatest.{MustMatchers, WordSpec}
import FormValidation._
import cats.data.Validated.{Invalid, Valid}

class FormValidationTest extends  WordSpec with MustMatchers {

  "getValue" in {
    val getName:FormData => ErrorsOr[String] = getValue("name")
    getName(Map("name" -> "myName")) mustBe Right("myName")
    getName(Map("surname" -> "myName")) mustBe Left(List("field name is not entered"))
  }

  "parseInt" in {
    parseInt("age")("11") mustBe Right(11)
    parseInt("age")("bla") mustBe Left(List("age must be an integer"))
  }

  "checks" in {
    nonBlank("name")("bla") mustBe Right("bla")
    nonBlank("name")("") mustBe Left(List("name cannot be blank"))
    nonNegative("age")(11) mustBe Right(11)
    nonNegative("age")(-1) mustBe Left(List("age must be nonnegative"))
  }

  "reads" in {
    readName(Map("name" -> "myName")) mustBe Right("myName")
    readName(Map("name" -> "")) mustBe Left(List("name cannot be blank"))
    readName(Map("age" -> "11")) mustBe Left(List("field name is not entered"))
  }

  "readUser" in {
    readUser(Map("name" -> "myName", "age" -> "50")) mustBe Valid(User("myName", 50))
    readUser(Map("age" -> "-1")) mustBe Invalid(List("field name is not entered", "age must be nonnegative"))
  }

}
