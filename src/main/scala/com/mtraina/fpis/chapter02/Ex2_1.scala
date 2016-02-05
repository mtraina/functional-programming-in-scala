package com.mtraina.fpis.chapter02

import scala.annotation.tailrec

class Ex2_1 {

  def fib(n: Int): Int = {
    @tailrec
    def go(n: Int, acc: Int, sum: Int): Int = {
      if(n == 0) acc
      else if(n == 1) sum
      else go(n-1, sum, acc+sum)
    }
    go(n, 0, 1)
  }

}
