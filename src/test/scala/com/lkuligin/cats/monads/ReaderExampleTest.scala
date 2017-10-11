package com.lkuligin.cats.monads

import com.lkuligin.cats.monads.ReaderExample._
import org.scalatest.{MustMatchers, WordSpec}

class ReaderExampleTest extends WordSpec with MustMatchers {
  val db = Db(
    Map(1 -> "userOne", 2 -> "userTwo", 3 -> "userThree"),
    Map("userOne" -> "pwdOne", "userTwo" -> "pwdTwo", "UserThree" -> "pwdThree"))

  "db example test" in {
    checkLogin(2, "pwdTwo").run(db) mustBe true
    checkLogin(2, "pwdWrong").run(db) mustBe false
    checkLogin(4, "pwd").run(db) mustBe false
  }

}
