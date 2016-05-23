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
      case Branch(l,r) => 1 + treeSize(l) + treeSize(r)
    }
  }

  def treeSize1[A](t: Tree[A]): Int = {
    fold(t)(i => 1)((i: Int, j: Int) => 1 + i + j)
  }

  def treeSize2[A](t: Tree[A]): Int = {
    fold(t)(i => 1)(1 + _ + _)
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

  def maximum2(t: Tree[Int]): Int = {
    fold(t)(i => i)((i: Int, j: Int) => i max j)
  }

  def maximum3(t: Tree[Int]): Int = {
    fold(t)(i => i)(_ max _)
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

  def depth1[A](t: Tree[A]): Int = {
    fold(t)(a => 0)((i, j) => 1 + (i max j))
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

  def map1[A](t: Tree[A])(f: A => A): Tree[A] = {
    fold(t)(a => Leaf(f(a)): Tree[A])((i, j) => Branch(i, j))
  }

  def map2[A](t: Tree[A])(f: A => A): Tree[A] = {
    fold(t)(a => Leaf(f(a)): Tree[A])(Branch(_, _))
  }

  /**
    * Ex. 3.29
    */
  def fold[A,B](t: Tree[A])(f: A => B)(g: (B,B) => B): B = {
    t match {
      case Leaf(v) => f(v)
      case Branch(l,r) => g(fold(l)(f)(g), fold(r)(f)(g))
    }
  }
}