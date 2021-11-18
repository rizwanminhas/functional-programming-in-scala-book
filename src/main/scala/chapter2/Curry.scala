package chapter2

object Curry extends App {
  def partial1[A, B, C](a: A, f: (A, B) => C): B => C = f(a, _)
  
  def curry[A, B, C](f: (A, B) => C): A => (B => C) =
    partial1(_, f) 

}