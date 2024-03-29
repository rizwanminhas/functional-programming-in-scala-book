package chapter2

object Compose extends App {
  def compose[A, B, C](f: B => C, g: A => B): A => C =
    (a: A) => f(g(a))
}