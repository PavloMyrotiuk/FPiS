package com.myrotiuk.chapter5

/**
 * Created by user on 06.01.2016.
 */
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
}




object Index extends App {
  //  println(Stream(1, 2, 3, 4, 5).take(2).toList)
  println(Stream(1, 2, 3, 4, 5).takeWhile(_ < 3).toList)
}

