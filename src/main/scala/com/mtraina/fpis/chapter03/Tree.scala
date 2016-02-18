package com.mtraina.fpis.chapter03

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  /**
    * Ex. 3.25
    */
  def size[A](t: Tree[A]): Int = {
    t match {
      case Leaf(v) => 1
      case Branch(l,r) => size(l) + size(r)
    }
  }
}