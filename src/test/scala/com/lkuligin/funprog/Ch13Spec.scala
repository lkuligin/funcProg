package com.lkuligin.funprog

import org.scalatest.{MustMatchers, WordSpec}
import com.lkuligin.funprog.Ch13.FreeImpl._
import com.lkuligin.funprog.Ch13.{FlatMap, Return}

class Ch13Spec extends WordSpec with MustMatchers {

  "Free monad" in {
    val monad = freeMonad[Option]
    monad.unit(1) mustBe Return(1)
  }

}
