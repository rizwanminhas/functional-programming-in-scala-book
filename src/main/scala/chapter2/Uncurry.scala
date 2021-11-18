package chapter2

object Uncurry extends App {
  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a: A, b: B) => f(a)(b)

  val a: Int = 3
  val b: Double = 1.0
  val c: String = "foo"

  val dToS: Double => String = (d: Double) => "dToS:" + d

  val iToDToS: Int => Double => String = (i: Int) => {
    println("iToDToS:" + i)
    dToS(_)
  }

  val res1 = uncurry(iToDToS)
  println(s"res1 ====> ${res1(a, b)}")
  
  println(iToDToS(a)(b))

  // or you can also use this built in method.
  val res2: (Int, Double) => String = Function.uncurried(iToDToS)
  println(s"res2 ====> ${res2(a, b)}")
}