package com.mtraina.fpis.chapter02

import scala.annotation.tailrec

object GettingStarted {

  def factorial(n: Int): Int = {
    @tailrec
    def go(n: Int, acc: Int): Int = {
      if(n <= 0) acc
      else go(n-1, n * acc)
    }
    go(n, 1)
  }

  /**
    * Exercise 2.1
    */
  def fib(n: Int): Int = {
    @tailrec
    def go(n: Int, acc: Int, sum: Int): Int = {
      if(n == 0) acc
      else if(n == 1) sum
      else go(n-1, sum, acc+sum)
    }
    go(n, 0, 1)
  }

  def formatResult(msg: String, n: Int, f: Int => Int): String = {
    msg.format(n, f(n))
  }

  // monomorphic function
  def findFirst(ss: Array[String], key: String): Int = {
    @tailrec
    def loop(n: Int): Int = {
      if(n >= ss.length) -1
      else if(ss(n) == key) n
      else loop(n + 1)
    }

    loop(0)
  }

  // polymorphic function
  def findFirst[A](ss: Array[A], p: A => Boolean): Int = {
    @tailrec
    def loop(n: Int): Int = {
      if(n >= ss.length) -1
      else if(p(ss(n))) n
      else loop(n + 1)
    }

    loop(0)
  }

  /**
    * Ex. 2.2
    */
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @tailrec
    def loop(n: Int): Boolean = {
      if(n >= as.length - 1) true
      else if(!ordered(as(n), as(n+1))) false
      else loop(n + 1)
    }

    loop(0)
  }

  def partial1[A,B,C](a: A, f: (A,B) => C): B => C = {
    (b: B) => f(a, b)
  }

  /**
    * Ex. 2.3
    */
  def curry[A,B,C](f: (A,B) => C): A => (B => C) = {
    a => b => f(a, b)
  }

  /**
    * Ex. 2.4
    */
  def uncurry[A,B,C](f: A => B => C): (A,B) => C = {
    (a, b) => f(a)(b)
  }

  /**
    * Ex. 2.5
    */
  def compose[A,B,C](f: B => C, g: A => B): A => C = {
    a => f(g(a))
  }
}
