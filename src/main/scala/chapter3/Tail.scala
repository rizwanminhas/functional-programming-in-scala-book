package chapter3

object Tail extends App {
  
  def tail[T](list: List[T]): List[T] = list match {
    case Nil => throw new NoSuchElementException("Nil.tail")
    case Cons(x, xs) => xs
  }
  
}