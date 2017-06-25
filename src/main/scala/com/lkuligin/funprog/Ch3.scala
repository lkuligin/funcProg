package com.lkuligin.funprog

/**
  * Created by lkuligin on 22/06/2017.
  */
sealed trait MyList[+A]
case object MyNil extends MyList[Nothing]
case class Cons[+A] (head: A, tail: MyList[A]) extends MyList[A]

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Ch3 {
  /**
    *3.2 Removes the first element of a List
    */
  def tail[A](lst: MyList[A]): MyList[A] = lst match {
    case MyNil => MyNil
    case Cons(_, tail) => tail
  }

  /**
    *  3.4 tail that removes n first elements of the list
    */
  @annotation.tailrec
  def tail[A](n: Int, lst: MyList[A]): MyList[A] = lst match {
    case MyNil => MyNil
    case Cons(_, tl) if n > 1 => tail(n-1, tl)
    case Cons(_, tl) => tl
  }

  /**
    * 3.3 Replaces the first element of the List
    */
  def setHead[A](h: A, lst: MyList[A]): MyList[A] = lst match {
    case MyNil => Cons(h, MyNil)
    case Cons(_, tail) => Cons(h, tail)
  }

  /**
   3.5 removes the elemnt from the List as long as they match a predicate
   */
  @annotation.tailrec
  def dropWhile[A](lst: MyList[A], f: A=>Boolean): MyList[A] = lst match {
    case MyNil => MyNil
    case Cons(head, tl) if f(head) => dropWhile(tl, f)
    case Cons(head, tl) => Cons(head, tl)
  }

  /**
    * 3.6 removes the last element from the list
    */
  def init[A](lst: MyList[A]): MyList[A] = lst match {
    case Cons(el, MyNil) => MyNil
    case Cons(head, tl) => Cons(head, init(tl))
    case MyNil => MyNil
  }

  def foldRight[A,B](as: MyList[A], z: B)(f: (A,B) => B): B = as match {
    case MyNil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  /**
    * 3.9 computes the length of the list with foldRight
    */
  def length[A](lst: MyList[A]): Int = foldRight(lst, 0)((_, n) => n+1)

  /**
    * Tail-rec foldLeft implementation
    */
  def foldLeft[A,B](lst: MyList[A], z: B)(f: (B,A) => B): B = {
    @annotation.tailrec
    def go(l: MyList[A], acc: B): B = l match {
      case MyNil => acc
      case Cons(el, tl) => go(tl, f(acc, el))
    }

    go(lst, z)
  }

  /**
    * 3.11 sum of the elements in the list using foldLefg
    */
  def sum(lst: MyList[Int]) = foldLeft(lst, 0)(_ + _)
  def product(lst: MyList[Int]) = foldLeft(lst, 1)(_ * _)


  /**
    * 3.12 reserse a list
    */
  def reverse[A](lst: MyList[A]): MyList[A] = {
    @annotation.tailrec
    def go(l: MyList[A], acc: MyList[A]): MyList[A] = l match {
      case MyNil => acc
      case Cons(h, tl) => go(tl, Cons(h, acc))
    }
    go(lst, MyNil)
  }

  /**
    * 3.13 implement foldLeft via foldRight
    */
  def foldLeftA[A,B](lst: MyList[A], z: B)(f: (B,A) => B): B =
    foldRight(lst, (b: B) => b)((a,g) => b => g(f(b,a)))(z)
  /*
  foldLeft([1,2,3], z)(f) = f(f(f(z,1),2),3)
  foldRight([1,2,3], z)(f) = f(1,f(2,f(3,z)))
  foldLeftA([1,2,3], z)(f) = g'(1,g'(2,g'(3,b=>b)))) = g'(1,g'(2,b=>f(b,3))))
    = g'(1,b=>f(f(b,2),3) = b=>f(f(f(b,1),2,3)
   */
  def foldRightA[A,B](lst: MyList[A], z:B)(f: (A,B) => B): B =
    foldLeft(lst, (b: B) => b)((g,a) => b => g(f(a,b)))(z)

  /**
    * 3.14 append in terms of foldLeft/foldRight
    */
  def append[A](lst1: MyList[A], lst2: MyList[A]): MyList[A] =
    foldRight(lst1, lst2)(Cons(_, _))
  //foldRight([1,2,3], [4,5,6])(Cons(_,_)) = Cons(1,Cons(2,Cons(3,[4,5,6])))

  /**
    * 3.15 concatenates a list of list into a single list
    */
  def concat[A](as: MyList[A]*): MyList[A] =
    if (as.isEmpty) MyNil
    else append(as.head, concat(as.tail: _*))

  def concatB[A](lst: MyList[MyList[A]]): MyList[A] =
    foldLeft(lst, MyList[A]())(append)

  def concatA[A](as: MyList[A]*) = foldLeft(MyList(as: _*), MyList[A]())(append)

  /**
    * 3.16 adds 1 to each element in MyList[Int]
    */
  def adds1(lst: MyList[Int]): MyList[Int] = lst match {
    case MyNil => MyNil
    case Cons(h, tl) => Cons(h+1, adds1(tl))
  }

  /**
    * 3.17 turns each value of MyList[Double] into a String
    */
  def doubleToString(lst: MyList[Double]): MyList[String] = lst match {
    case MyNil => MyNil
    case Cons(h,tl) => Cons(h.toString, doubleToString(tl))
  }

  /**
  * 3.18 write a map function
   */
  def map[A,B](as: MyList[A])(f: A => B): MyList[B] = as match {
    case MyNil => MyNil
    case Cons(h, tl) => Cons(f(h), map(tl)(f))
  }

  /**
    * 3.19
    */
  def filter[A](as: MyList[A])(f: A => Boolean): MyList[A] = as match {
    case MyNil => MyNil
    case Cons(h, tl) if (f(h)) => Cons(h, filter(tl)(f))
    case Cons(h, tl) => filter(tl)(f)
  }

  /**
    * 3.20
    *
    */
  def flatMap[A,B](as: MyList[A])(f: A => MyList[B]): MyList[B] = as match {
    case MyNil => MyNil
    case Cons(h, tl) => append(f(h), flatMap(tl)(f))
  }

  /**
    * 3.21 use flatMap to implement filter
    */
  def filterA[A](as: MyList[A])(f: A => Boolean): MyList[A] = flatMap(as)(el => if (f(el)) MyList(el) else MyNil)

  /**
    * 3.22 accepts 2 lists and construct a new list by adding a corresponding element
    */
  def zip(lst1: MyList[Int], lst2: MyList[Int]): MyList[Int] = (lst1, lst2) match {
    case (MyNil, MyNil) => MyNil
    case (MyNil, _) => throw new IllegalArgumentException("lists should have the same length")
    case (_, MyNil) => throw new IllegalArgumentException("lists should have the same length")
    case (Cons(h1, tl1), Cons(h2, tl2)) => Cons(h1+h2, zip(tl1, tl2))
  }

  /**
    * 3.23
    */
  def zipWith[A, B](lst1: MyList[A], lst2: MyList[A])(f: (A, A) => B): MyList[B] = (lst1, lst2) match {
    case (MyNil, MyNil) => MyNil
    case (MyNil, _) => throw new IllegalArgumentException("lists should have the same length")
    case (_, MyNil) => throw new IllegalArgumentException("lists should have the same length")
    case (Cons(h1, tl1), Cons(h2, tl2)) => Cons(f(h1,h2), zipWith(tl1, tl2)(f))
  }

  /**
    * 3.24 checks whether a List contains another List as a subsequence
    */
  @annotation.tailrec
  def hasSubSequence[A](sup: List[A], sub: List[A]): Boolean = {
    //very inefficient!
    (sup.length >= sub.length) && ((sup.take(sub.length) == sub) || hasSubSequence(sup.tail, sub))
  }

  /**
    * 3.25 count the number of nodes (leaves and branches) in a tree
    */
  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(left, right) => 1 + size(left) + size(right)
  }

  /**
    * 3.26 returns maximum element in Tree[Int]
    */
  def maxEl(tree: Tree[Int]): Int = tree match {
    case Leaf(x) => x
    case Branch(left, right) => maxEl(left) max maxEl(right)
  }

  /**
    * 3.27 depth of the tree
    */
  def depth[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(left, right) => 1 + (depth(left) max depth(right))
  }

  /**
    * 3.28 map for Tree
    */
  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(l) => Leaf(f(l))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }

  /**
    * 3.29 fold
    */
  def fold[A,B](tree: Tree[A])(f1: A => B)(f2: (B,B) => B): B = tree match {
    case Leaf(l) => f1(l)
    case Branch(left, right) => f2(fold(left)(f1)(f2), fold(right)(f1)(f2))
  }

  def sizeA[A](tree: Tree[A]): Int = fold(tree)(_ => 1)( _ + _ + 1)
  def maxElA(tree: Tree[Int]): Int = fold[Int, Int](tree)(x => x)(_ max _)
  def depthA[A](tree: Tree[A]): Int = fold[A, Int](tree)(_ => 1)(1 + _ max _)
  def mapA[A,B](tree: Tree[A])(f: A => B): Tree[B] = fold[A,Tree[B]](tree)(x => Leaf(f(x)))(Branch(_,_))

}

object MyList {
  def apply[A](as: A*): MyList[A] =
    if (as.isEmpty) MyNil
    else Cons(as.head, apply(as.tail: _*))

}
