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
  type Rand[+A] = RNG => (A, RNG)

  val int2: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def positiveMax(n: Int): Rand[Int] = map(positiveInt)(i => n - i % n)

  val double2: Rand[Double] = map(int2)(_.toDouble)

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rngA) = ra(rng)
      val (b, rngB) = rb(rngA)
      (f(a, b), rngB)
    }

  def intDouble2(rng: RNG): Rand[(Int, Double)] = map2(int2, double2)((a, b) => a -> b)

  def doubleInt2(rng: RNG): Rand[(Double, Int)] = map2(double2, int2)((a, b) => a -> b)

//  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
//
//    def sequence[A](acc: List[A])(fs: List[Rand[A]]): Rand[List[A]] = {
//      fs match {
//        case Nil => rng => (acc, rng)
//        case x :: xs => {
//          (rng => (a, _)) = x(rng)
//          sequence[A](a :: acc)(xs)
//        }
//      }
//    }
//    rng => sequence(List.empty[A])(fs)
//  }
}

object IndexRNG extends App {

}