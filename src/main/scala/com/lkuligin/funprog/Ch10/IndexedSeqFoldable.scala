package com.lkuligin.funprog.Ch10

/**
  * 10.12 implement Foldable[IndexedSeq]
  */
object IndexedSeqFoldable extends Foldable[IndexedSeq] {
  def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)

  def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)

  override def foldMap[A, B](as: IndexedSeq[A])(z: B)(f: A => B)(m: Monoid[B]): B = m.op(z, Monoids.foldMapV(as, m)(f)) //we improve performance using foldMapV
}
