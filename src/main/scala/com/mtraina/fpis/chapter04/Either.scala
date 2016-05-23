package com.mtraina.fpis.chapter04

sealed trait Either[+E, +A] {

  /**
    * Ex. 4.6
    */
  def map[B](f: A => B): Either[E, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => Right(f(a))
  }

  /**
    * Ex. 4.6
    */
  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => f(a)
  }

  /**
    * Ex. 4.6
    */
  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(e) => b
    case Right(a) => this
  }

  /**
    * Ex. 4.6
    */
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    for {
      a <- this
      bb <- b
    } yield f(a, bb)
}

case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Either {

  /**
    * Ex. 4.7
    */
  def traverse[E, A, B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    es match {
      case Nil => Right(Nil)
      case h::t => (f(h) map2 traverse(t)(f))(_ :: _)
    }

  /**
    * Ex. 4.7
    */
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = traverse(es)(e => e)
}