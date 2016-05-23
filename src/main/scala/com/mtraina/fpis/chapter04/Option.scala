package com.mtraina.fpis.chapter04

sealed trait Option[+A] {

  /**
    * Ex. 4.1
    */
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(x) => Some(f(x))
  }

  /**
    * Ex. 4.1
    */
  def flatMap[B](f: A => Option[B]): Option[B] = map(f) getOrElse None

  /**
    * Ex. 4.1
    */
  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(x) => x
  }

  /**
    * Ex. 4.1
    */
  def orElse[B >: A](ob: => Option[B]): Option[B] = map(Some(_)) getOrElse ob

  /**
    * Ex. 4.1
    */
  def filter(f: A => Boolean): Option[A] = if(map(f) getOrElse false) this else None
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  /**
    * Ex. 4.2
    */
  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))

  /**
    * Ex. 4.3
    */
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A,B) => C): Option[C] =
    a flatMap(aa => b map (bb => f(aa, bb)))

  /**
    * Ex. 4.3
    * map2 implementation using for-comprehension
    */
  def map2_1[A,B,C](a: Option[A], b: Option[B])(f: (A,B) => C): Option[C] =
    for {
      aa <- a
      bb <- b
    } yield f(aa, bb)

  /**
    * Ex. 4.4
    */
  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
      case Nil => Some(Nil)
      case h :: t => h flatMap(hh => sequence(t) map (oas => hh :: oas))
  }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil => Some(Nil)
    case h :: t => map2(f(h), traverse(t)(f))((hh, tt) => hh :: tt)
  }
}