package chapter2

import scala.annotation.tailrec
import scala.collection.mutable.HashMap

object Fibonacci extends App {
  def fibonacci(n: Int): BigInt = {
    @tailrec
    def go(n: Int, a: BigInt, b: BigInt): BigInt =
      if (n < 1) a
      else go(n - 1, b, a + b)
    go(n, 0, 1)
  }

  def fibonacciPM(n: Int): BigInt = {
    @tailrec
    def go(n: Int, a: BigInt, b: BigInt): BigInt = n match {
      case 0 => a
      case 1 => b
      case _ => go(n - 1, b, a + b)
    }
    go(n, 0, 1)
  }

  val fibonacciStream1: Stream[BigInt] =
    0 #:: fibonacciStream1.scan(BigInt(1))(_ + _)

  val fibonacciStream2: Stream[BigInt] =
    BigInt(0) #:: BigInt(1) #:: fibonacciStream2
      .zip(fibonacciStream2.tail)
      .map(n => n._1 + n._2)

  val fibonacciStream3: Stream[BigInt] = {
    def fs(prev: BigInt, curr: BigInt): Stream[BigInt] =
      prev #:: fs(curr, prev + curr)
    fs(0, 1)
  }

  val fibonacciMemoized: Int => BigInt = memoize(n =>
    n match {
      case 0 => 0
      case 1 => 1
      case _ => fibonacciMemoized(n - 2) + fibonacciMemoized(n - 1)
    }
  )

  def memoize[K, V](f: K => V): K => V = new HashMap[K, V] {
    override def apply(a: K) = getOrElseUpdate(a, f(a))
  }

  def time(block: => Unit): Unit = {
    val start = System.currentTimeMillis
    block
    println(s"time taken ${System.currentTimeMillis - start} ms")
  }

  val range = 0 to 100

  time {
    println("fibonacciStream1")
    range.foreach(fibonacciStream1)
  }

  time {
    println("fibonacciStream2")
    range.foreach(fibonacciStream2)
  }

  time {
    println("fibonacciStream3")
    range.foreach(fibonacciStream3)
  }

  time {
    println("fibonacciMemoized")
    range.foreach(fibonacciMemoized)
  }

  time {
    println("fibonacciPM")
    range.foreach(fibonacciPM)
  }

  time {
    println("fibonacci")
    range.foreach(fibonacci)
  }

}
