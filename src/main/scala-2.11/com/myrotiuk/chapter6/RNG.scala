package com.myrotiuk.chapter6

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {

  def positiveInt(rng: RNG): (Int, RNG) = {
    rng.nextInt match {
      case (result, generator) if (result == Int.MinValue) => positiveInt(generator)
      case (result, generator) if (result < 0) => (result.abs, generator)
      case (result, generator) => (result, generator)
    }
  }

  def double(rng: RNG): (Double, RNG) = ((positiveInt(rng)._1 - 1).abs / Int.MaxValue, positiveInt(rng)._2)

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (r1, rng1) = positiveInt(rng)
    val (r2, rng2) = double(rng1)
    r1 -> r2 -> rng2
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (r1, rng1) = double(rng)
    val (r2, rng2) = positiveInt(rng1)
    r1 -> r2 -> rng2
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (r1, rng1) = double(rng)
    val (r2, rng2) = double(rng1)
    val (r3, rng3) = double(rng2)
    (r1, r2, r3) -> rng3
  }

//  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
//    if (count == 0) {
//      Nil -> rng
//    } else {
//      val (next, rng1) = positiveInt(rng)
//      val (intss, finalRng) = ints(count - 1)(rng1)
//      next::intss -> finalRng
//    }
//  }
}

object IndexRNG extends App {

}