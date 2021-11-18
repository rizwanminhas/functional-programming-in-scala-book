package chapter2

object BinarySearch extends App {
  def binarySearch(ds: Array[Double], key: Double): Int = {
    @annotation.tailrec
    def go(low: Int, high: Int): Int = {
      if (low > high) -1
      else {
        val mid = (low + high) / 2
        val d = ds(mid)
        if (d == key) mid
        else if (d > key) go(low, mid - 1)
        else go(mid + 1, high)
      }
    }
    go(0, ds.length - 1)
  }

  def binarySearch[A](ds: Array[A], key: A, gt: (A,A) => Boolean): Int = {
    @annotation.tailrec
    def go(low: Int, high: Int): Int = {
      if (low > high) -1
      else {
        val mid = (low + high) / 2
        val d = ds(mid)
        val greater = gt(d, key)
        if (!greater && !gt(key, d)) mid
        else if (greater) go(low, mid - 1)
        else go(mid + 1, high)
      }
    }
    go(0, ds.length - 1)
  }

  //println(binarySearch(Array(1.0, 2.0, 3.7), 2.0))
  println(binarySearch(Array(1.0, 2.0, 3.7), 3.0, (a: Double, b: Double) => a > b))
}