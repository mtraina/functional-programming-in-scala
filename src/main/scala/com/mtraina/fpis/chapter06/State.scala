package com.mtraina.fpis.chapter06

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = Simple(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val next = rng.nextInt
    if(next._1 == Int.MinValue) (-1 * (next._1 + 1), rng)
    else if(next._1 < 0)(-1 * next._1, rng)
    else next
  }

}