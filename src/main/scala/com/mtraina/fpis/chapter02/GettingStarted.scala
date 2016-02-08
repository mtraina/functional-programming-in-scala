package com.mtraina.fpis.chapter02

import scala.annotation.tailrec

object GettingStarted {

  def factorial(n: Int): Int = {

    def go(n: Int, acc: Int): Int = {
      if(n <= 0) acc
      else go(n-1, n * acc)
    }
    go(n, 1)
  }

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

  def findFirst(ss: Array[String], key: String): Int = {
    @tailrec
    def loop(n: Int): Int = {
      if(n >= ss.length) -1
      else if(ss(n) == key) n
      else loop(n + 1)
    }

    loop(0)
  }

}
