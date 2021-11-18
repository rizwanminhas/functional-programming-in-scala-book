package chapter2

import scala.annotation.tailrec

object Factorial extends App {
  
  def factorial(n: Int): Int = {
    @tailrec
    def go(n: Int, acc: Int): Int = 
      if (n <= 0) acc
      else go(n-1, n * acc)
    go(n, 1)
  }
  
  (0 to 10).foreach(n => println(f"$n%2d factorial = ${factorial(n)}"))
}