package com.mtraina.fpis.chapter03

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if(as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  /**
    * Ex. 3.1
    */
  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  /**
    * Ex. 3.2
    */
  def tail[A](l: List[A]): List[A] = {
    l match {
      case Nil => Nil
      case (Cons(_, t)) => t
    }
  }

  /**
    * Ex. 3.3
    */
  def setHead[A](head: A, l: List[A]): List[A] = {
    l match {
      case Nil => Cons(head, Nil)
      case _ => Cons(head, l)
    }
  }

  /**
    * Ex. 3.4
    */
  def drop[A](n: Int, l: List[A]): List[A] = {
    if(n <= 0) l
    else drop(n - 1, tail(l))
  }

  /**
    * Ex. 3.5
    */
  def dropWhile[A](l: List[A], p: A => Boolean): List[A] = {
    l match {
      case Nil => l
      case Cons(h,t) => if(!p(h)) Cons(h, dropWhile(t, p)) else dropWhile(t, p)
    }
  }

  def dropWhile2[A](as: List[A])(f: A => Boolean): List[A] =
    as match {
      case Cons(h, t) if f(h) => dropWhile2(t)(f)
      case _ => as
    }

  /**
    * Ex. 3.6
    */
  def init[A](l: List[A]): List[A] = {
    l match {
      case Nil => Nil
      case Cons(h, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }
  }

  def foldRight[A,B](as: List[A], z: B)(f: (A,B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) = foldRight(ns, 0)((x, y)=> x + y)

  def product2(ns: List[Double]) = foldRight(ns, 1.0)(_ * _)
}
