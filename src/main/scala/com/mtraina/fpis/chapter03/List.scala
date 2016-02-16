package com.mtraina.fpis.chapter03

import scala.annotation.tailrec

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

  def sum2(ns: List[Int]) = foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) = foldRight(ns, 1.0)(_ * _)

  /**
    * Ex. 3.7
    *
    * No because for every call of f we evaluate its arguments, so we need to
    * evaluate foldRight again for each element of the list.
    */

  /**
    * Ex. 3.8 and Ex. 3.14
    */
  def append[A](a: List[A], b: List[A]): List[A] = foldRight(a, b)(Cons(_ , _))

  /**
    * Ex. 3.9
    */
  def length[A](l: List[A]): Int = foldRight(l, 0)((x, y) => y + 1)

  /**
    * Ex. 3.10
    */
  @tailrec
  def foldLeft[A,B](as: List[A], z: B)(f: (B,A) => B): B = {
    as match {
      case Nil => z
      case Cons(h,t) => foldLeft(t, f(z, h))(f)
    }
  }

  /**
    * Ex. 3.11
    */
  def sum3(is: List[Int]): Int = foldLeft(is, 0)(_ + _)
  def product3(ds: List[Double]): Double = foldLeft(ds, 1.0)(_ * _)

  /**
    * Ex. 3.12
    */
  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil: List[A])((x, y) => Cons(y, x))

  /**
    * Ex. 3.15
    */
  def concat[A](l: List[List[A]]): List[A] = {
    foldRight(l, Nil: List[A])(append)
  }

  /**
    * Ex. 3.16
    */
  def add1(is: List[Int]): List[Int] = foldRight(is, Nil: List[Int])((x, y) => Cons(x + 1, y))

  /**
    * Ex. 3.17
    */
  def doubleToString(ds: List[Double]): List[String] = foldRight(ds, Nil: List[String])((x, y) => Cons(x.toString, y))

  /**
    * Ex. 3.18
    */
  def map[A,B](as: List[A])(f: A => B): List[B] = foldRight(as, Nil: List[B])((x,y) => Cons(f(x), y))

  /**
    * Ex. 3.19
    */
  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    foldRight(as, Nil: List[A])((x,y) => if (f(x)) Cons(x, y) else y)
  }

  /**
    * Ex. 3.20
    */
  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = foldRight(as, Nil: List[B])((x,y) => append(f(x), y))
  def flatMap1[A,B](as: List[A])(f: A => List[B]): List[B] = concat(map(as)(f))
}
