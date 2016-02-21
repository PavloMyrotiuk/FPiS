package com.myrotiuk.chapter6

trait RNG {
  def nextInt: (Int, RNG)

  def positiveInt: (Int, RNG) = {
    nextInt match {
      case (result, generator) if (result == Int.MinValue) => generator.positiveInt
      case (result, generator) if (result < 0) => (result.abs, generator)
      case (result, generator) => (result, generator)
    }
  }
}

object RNG {
  def double(rng: RNG): (Double, RNG) = ((rng.positiveInt._1 - 1).abs / Int.MaxValue, rng.positiveInt._2)

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val int: (Int, RNG) = rng.nextInt
    val double: (Double, RNG) = RNG.double(int._2)
    ((int, double), double._2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = ???

  def double3(rng: RNG): ((Double, Double, Double), RNG) = ???
}

object IndexRNG extends App {

}