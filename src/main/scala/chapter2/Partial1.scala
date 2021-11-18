package chapter2

object Partial1 extends App {

  def partial1[A, B, C](a: A, f: (A, B) => C): B => C = f(a, _)

  val a: Int = 1
  val b: Double = 2.0

  val f: (Int, Double) => String = (n, d) => s"you passed $n and $d"

  val ret = partial1(a, f)
  println("ret:" + ret)
  
  val c = ret(b)
  println("c:" + c)
}