package chapter3

import scala.annotation.tailrec

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List extends App {
  def sum(ints: List[Int]): Int = ints match {
    case Nil         => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil          => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs)  => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[T](list: List[T]): List[T] = list match {
    case Nil         => throw new NoSuchElementException("Nil.tail")
    case Cons(x, xs) => xs
  }

  def drop[A](list: List[A], n: Int): List[A] = {
    if (n <= 0)
      list
    else
      list match {
        case Nil         => Nil
        case Cons(x, xs) => drop(xs, n - 1)
      }
  }

  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = {
    l match {
      case Nil         => Nil
      case Cons(x, xs) => if (f(x)) dropWhile(xs)(f) else l
    }
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil         => Nil
    case Cons(x, xs) => Cons(h, xs)
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil        => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def init1[A](l: List[A]): List[A] = l match {
    case Nil          => Nil
    case Cons(_, Nil) => Nil
    case Cons(h, t)   => Cons(h, init1(t))
  }

//  def init2[A](l: List[A]): List[A] = {
//    import collection.mutable.ListBuffer
//    val buf = new ListBuffer[A]
//    @tailrec
//    def go(cur: List[A]): List[A] = cur match {
//      case Nil          => Nil
//      case Cons(_, Nil) => List(buf: _*)
//      case Cons(h, t)   => buf += h; go(t)
//    }
//    go(l)
//  }

  def foldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    l match {
      case Nil         => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  println(foldRight(List(1, 2, 3, 4), 0)(_ + _))
  println(foldRight(List(1, 2, 3, 4), 1)(_ * _))

  val example = Cons(1, Cons(2, Cons(3, Nil)))
  val example2 = List(1, 2, 3)
  val total = sum(example)

  println(example)
  println(example2)
  println(total)

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _)))          => x
    case Nil                                   => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t)                            => h + sum(t)
    case _                                     => 101
  }

  println(x)
  println(init1(List(1, 2, 3, 4)))
  println(foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _)))

  def length[A](l: List[A]): Int =
    foldRight(l, 0)((a, acc) => acc + 1)

  @tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil        => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  println(foldLeft(List(1, 2, 3, 4), 0)(_ + _))
  println(foldLeft(List(1, 2, 3, 4), 1)(_ * _))
  println(foldLeft(List(1, 2, 3, 4), 0)((acc, _) => acc + 1))
  
  println(foldLeft(List(1,2,3,4), List[Int]())((acc, h) => Cons(h, acc)))
}