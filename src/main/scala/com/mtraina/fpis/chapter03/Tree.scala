package com.mtraina.fpis.chapter03

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  /**
    * Ex. 3.25
    */
  def treeSize[A](t: Tree[A]): Int = {
    t match {
      case Leaf(v) => 1
      case Branch(l,r) => treeSize(l) + treeSize(r)
    }
  }

  /**
    * Ex. 3.26
    */
  def maximum(t: Tree[Int]): Int = {
    def helper(maxl: Int, maxR: Int) = if(maxl > maxR) maxl else maxR

    t match {
      case Leaf(v) => v
      case Branch(l,r) => helper(maximum(l), maximum(r))
    }
  }

  def maximum1(t: Tree[Int]): Int = {
    t match {
      case Leaf(v) => v
      case Branch(l,r) => maximum(l) max maximum(r)
    }
  }

  /**
    * Ex. 3.27
    */
  def depth[A](t: Tree[A]): Int = {
    t match {
      case Leaf(_) => 0
      case Branch(l,r) => 1 + (depth(l) max depth(r))
    }
  }

  /**
    * Ex. 3.28
    */
  def map[A](t: Tree[A])(f: A => A): Tree[A] = {
    t match {
      case Leaf(v) => Leaf(f(v))
      case Branch(l,r) => Branch(map(l)(f), map(r)(f))
    }
  }

  def fold[A,B](t: Tree[A])(f: A => B)(g: (B,B) => B): B = {
    t match {
      case Leaf(v) => f(v)
      case Branch(l,r) => g(fold(l)(f)(g), fold(r)(f)(g))
    }
  }
}