package co.nochy.fpinscala.chapter02

object MyModule {

  def abs(n: Int): Int =
    if (n < 0) -n else n

  private def formatAbs(x: Int) = {
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))
  }

  def factorial(n: Int) : Int = {

    def go(n: Int, acc: Int): Int =
      if (n  <= 0) acc
      else go(n - 1, n * acc)

    go(n, 1)

  }

  private def formatFanctorial(x: Int) = {
    val msg = "The fanctorial of %d is %d"
    msg.format(x, factorial(x))
  }

  def findFirst(ds: Array[Double], key: Double): Int = {
    def loop(n: Int): Int =
      if (n >= ds.length) -1
      else if (ds(n) == key) n
      else loop(n + 1)

    loop(0)
  }

  def fib(n: Int): Int = n match {
    case 0 => 0
    case 1 => 1
    case _ => fib(n - 2) + fib(n - 1)
  }

  def isSorted[A](as: Array[A], gt: (A,A) => Boolean): Boolean = {
    def loop(index: Int): Boolean =
      if (index >= as.length - 1) true
      else if (gt(as(index), as(index + 1))) loop(index + 1)
      else false

    loop(0)
  }

  def partial1[A,B,C](a: A, f: (A, B) => C): B => C = (b: B) => f(a, b)

  def curry[A,B,C](f: (A, B) => C): A => (B => C) = a => b => f(a, b)

  def uncurry[A,B,C](f: A => B => C): (A, B) => C = (a, b) => f(a)(b)

  def compose[A,B,C](f: B => C, g: A => B): A => C = (a: A) => f(g(a))

  def main(args: Array[String]): Unit =
  println(formatAbs(-42))
  println(formatFanctorial((7)))
}
