package com.mtraina.fpis.chapter08

import org.scalacheck.Gen
import org.scalacheck.Prop.forAll

object PropertyTest {

  def checkList = {
    val intList = Gen.listOf(Gen.choose(0,100))
    val prop = forAll(intList)(ns => ns.reverse.reverse == ns) &&
      forAll(intList)(ns => ns.headOption == ns.reverse.lastOption)
    val failingProp = forAll(intList)(ns => ns.reverse == ns)
  }

}
