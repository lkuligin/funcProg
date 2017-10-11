package com.lkuligin.cats.foldables

object FoldableOps {

  def mapF[A, B](list: List[A])(f: A => B): List[B] = list.foldLeft(List[B]())((acc, el) => f(el) :: acc).reverse

  def flatMapF[A,B](list: List[A])(f: A => List[B]): List[B] = list.foldLeft(List[B]())((acc, el) => f(el).reverse ::: acc).reverse

  def filterF[A](list: List[A])(f: A => Boolean): List[A] = list.foldLeft(List[A]())((acc, el) => if (f(el)) el :: acc else acc).reverse

  def reverseF[A](list: List[A]) = list.foldLeft(List[A]())((acc, el) => el :: acc)

}
