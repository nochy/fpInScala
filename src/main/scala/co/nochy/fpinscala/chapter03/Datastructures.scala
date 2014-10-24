package co.nochy.fpinscala.chapter03

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]
object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }
  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](l: List[A]): List[A] = l match {
    case (Cons(_, x)) => x
    case _ => Nil
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case (Cons(x, y)) => Cons(h, y)
    case _ => Nil
  }

  // deleteになってた…
  def drop[A](l: List[A], n: Int): List[A] =
    if (n <= 0) {
      l
    } else if (n == 1) {
      tail(l)
    } else {
      l match {
        case Cons(x, y) => Cons(x, drop(y, n -1))
        case _ => Nil
      }
    }

  def drop2[A](l: List[A], n: Int): List[A] =
    if (n <= 0) {
      l
    } else {
      l match {
        case Cons(_, y) => drop(y, n-1)
        case _ => Nil
      }
    }

}

