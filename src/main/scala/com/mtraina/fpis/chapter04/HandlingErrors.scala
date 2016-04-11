package com.mtraina.fpis.chapter04

class HandlingErrors {
  sealed trait Option[+A]
  case class Some[+A](get: A) extends Option[A]
  case object None extends Option[Nothing]
}
