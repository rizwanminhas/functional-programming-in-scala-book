package chapter2

import scala.annotation.tailrec

object IsSorted extends App {

  def isSorted[@specialized A](as: Array[A], gt: (A, A) => Boolean): Boolean = {
    @tailrec
    def go(n: Int, len: Int): Boolean = {
      if (n == len - 1) true
      else if (gt(as(n), as(n + 1))) true
      else go(n + 1, len)
    }
    go(0, as.length)
  }

  println(isSorted(Array(1, 2, 3, 4, 5), (a: Int, b: Int) => a < b))

  def isSorted[A](seq: Seq[A], gt: (A, A) => Boolean): Boolean = {
    seq.length match {
      case 0 | 1 => true
      case _     => seq.sliding(2).exists(tuple => gt(tuple(0), tuple(1)))
    }
  }

  val arr = List(3, 2)

  println(isSorted(arr, (a: Int, b: Int) => a < b))

}