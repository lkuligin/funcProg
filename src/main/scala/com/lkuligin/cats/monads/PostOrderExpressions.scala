package com.lkuligin.cats.monads

import cats.data.State
import cats.syntax.applicative._

object PostOrderExpressions {
  type CalcState[A] = State[List[Int], A]

  def operator(operatorFunc: (Int, Int) => Int): CalcState[Int] =
    State[List[Int], Int] {
      case first :: second :: tail => {
        val res = operatorFunc(first, second)
        (res :: tail, res)
      }
      case _ => sys.error("Failed!")
    }

  def operand(value: Int): CalcState[Int] =
    State[List[Int], Int] {
      x => (value :: x, value)
    }

  def evalOne(sym: String): CalcState[Int] = sym match {
    case "+" => operator(_ + _)
    case "-" => operator(_ - _)
    case "*" => operator(_ * _)
    case "/" => operator(_ / _)
    case value => operand(value.toInt)
  }

  def evalAll(input: List[String]): CalcState[Int] = input.foldLeft(0.pure[CalcState])((x,y) => x.flatMap(_ => evalOne(y)))

}
