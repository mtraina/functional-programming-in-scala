package com.mtraina.fpis.chapter03

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](lef: Tree[A], right: Tree[A]) extends Tree[A]
