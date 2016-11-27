package com.mtraina.fpis.chapter06

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {

  case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  /**
    * Ex. 6.1
    */
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    if(i < 0) (-i - 1, r) else (i, r)
  }

  /**
    * Ex. 6.2
    */
  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    (i / (Int.MaxValue.toDouble + 1), r)
  }

  /**
    * Ex. 6.3
    */
  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val i = rng.nextInt
    val d = double(rng)
    ((i._1, d._1), d._2)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((i, d), r) = intDouble(rng)
    ((d, i), r)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  /**
    * Ex. 6.4
    */
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if(count == 0){
      (List(), rng)
    } else {
      val (i1, r1) = rng.nextInt
      val (is, r2) = ints(count - 1)(r1)
      (i1 :: is, r2)
    }
  }

  // better API for state actions
  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] = rng => {
    val (a, rng2) = s(rng)
    (f(a), rng2)
  }

  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)

  /**
    * Ex. 6.5
    */
  def doubleMap: Rand[Double] = map(nonNegativeInt)(i => i / (Int.MaxValue.toDouble + 1))

  /**
    * Ex. 6.6
    */
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = ???

  /**
    * Ex. 6.7
    */
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = ???

}