package com.myrotiuk.chapter5

object Stream {
  def empty[A]: Stream[A] = new Stream[A] {
    def uncons = None
  }

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = new Stream[A] {
    lazy val uncons = Some((hd, tl))
  }

  def merge[A](one: => Stream[A], another: => Stream[A]): Stream[A] = {
    if (one.isEmpty) another
    else cons(one.uncons.get._1, merge(one.uncons.get._2, another))
  }

  def append[A](st: => Stream[A], el: => A): Stream[A] = merge(st, Stream(el));

  def apply[A](as: A*): Stream[A] = if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  def fibs: Stream[Int] = {
    def fibs(a: Int, b: Int): Stream[Int] = cons(a, fibs(b, a + b))
    fibs(0, 1)
  }

  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))

  def from(n: Int): Stream[Int] = Stream.cons(n, Stream.from(n + 1))

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((h, t)) => Stream.cons(h, Stream.unfold(t)(f))
    case None => Stream.empty
  }

  def from2(n: Int): Stream[Int] = unfold(n)(n => Some(n, n + 1))

  def constant2[A](a: A): Stream[A] = unfold(a)(_ => Some(a, a))

  def fibs2: Stream[Int] = unfold(0 -> 1)(s => Some(s._1, s._1 + s._2 -> s._1))

}

trait Stream[+A] {
  def uncons: Option[(A, Stream[A])]

  def isEmpty: Boolean = uncons.isEmpty

  def toList: List[A] = {
    def toList(uncons: Option[(A, Stream[A])]): List[A] = uncons match {
      case None => Nil
      case Some((hd, tail)) => hd :: toList(tail.uncons)
    }

    toList(uncons)
  }

  def take(n: Int): Stream[A] = {
    def take(n: Int, given: Stream[A], acc: Stream[A]): Stream[A] = {
      if (given.isEmpty || n == 0) acc
      else take(n - 1, given.uncons.get._2, Stream.append(acc, given.uncons.get._1))
    }
    take(n, this, Stream.empty)
  }

  def takeWhile(p: A => Boolean): Stream[A] = {
    def take(given: Stream[A], acc: Stream[A])(implicit p: A => Boolean): Stream[A] = {
      val tuple: (A, Stream[A]) = given.uncons.get

      if (given.isEmpty || !p(tuple._1)) acc
      else take(tuple._2, Stream.append(acc, tuple._1))
    }
    take(this, Stream.empty)(p)
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    uncons match {
      case Some((h, t)) => f(h, t.foldRight(z)(f))
      case None => z
    }

  def exists(p: A => Boolean): Boolean = foldRight(false)((a, b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

  def takeWhile2(p: A => Boolean): Stream[A] = foldRight(Stream.empty[A]) { (a, b) => if (p(a)) Stream.cons(a, b) else Stream.empty }

  def map[B](p: A => B): Stream[B] = foldRight(Stream.empty[B])((a, b) => Stream.cons(p(a), b))

  def filter(p: A => Boolean): Stream[A] = foldRight(Stream.empty[A]) { (a, b) => if (p(a)) Stream.cons(a, b) else b }

  def flatMap[B](p: A => Stream[B]): Stream[B] = foldRight(Stream.empty[B])((a, b) => Stream.cons(p(a).uncons.get._1, b))
}


object Index extends App {
  //  println(Stream(1, 2, 3, 4, 5).take(2).toList)
  //    println(Stream(1, 2, 3, 4, 5,2).takeWhile(_==1).toList)
  //  println(Stream(1, 2, 4, 5, 6, 7, 8, 9, 10).forAll(_ < 10))
  //  println(Stream(1, 2, 3, 4, 5, 1, 8).takeWhile2(_ < 3).toList)
  //  println(Stream(1, 2, 3, 4, 5, 1, 8).filter(_ < 5).toList)
  //  println(Stream(1, 2, 3).map(1 + _).toList)
  //  private val stream: Stream[Int]= for (i <- Stream(1, 2, 3); j<- Stream(1, 2,3) if i*j < 40) yield i*j
  //  println(stream.toList)
  //  println(Stream(1, 2, 3, 4).map(_ + 10).filter(_ % 2 == 0).toList)
  //  val ones: Stream[Int] = Stream.cons(1, ones)
  //  println(ones.take(5).toList)
  //  println(ones.takeWhile(_==1).toList)
  //  println(ones.forAll(_ != 1))

  //    println(Stream.constant(6).take(4).toList)
  //  println(Stream.from(7).take(10).toList)
  //  println(Stream.unfold(5)(t => Some("five", t)).take(7).toList)
  //  println(Stream.from2(3).take(5).toList)
  //  println(Stream.constant2("A").take(4).toList)
  //  println(Stream.fibs.take(5).toList)
  println(Stream.fibs.take(10).toList)
  println(Stream.fibs2.take(10).toList)
}
