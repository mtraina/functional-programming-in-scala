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
  def dropWhile[A](p: A => Boolean, l: List[A]): List[A] = {

    def inner(p: A => Boolean, l: List[A], r: List[A]): List[A] = {
      r match {
        case Nil => l
        case Cons(h, t) => if(!p(h)) inner(p, Cons(h, l), t) else inner(p, l, t)
      }
    }

    inner(p, Nil, l)
  }
}
