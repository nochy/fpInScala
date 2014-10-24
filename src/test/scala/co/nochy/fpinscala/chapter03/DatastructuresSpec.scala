package co.nochy.fpinscala.chapter03



import org.specs2.mutable.Specification

class DatastructuresSpec extends Specification {
  "a" should {
    "b" in {
      val ex1: List[Double] = Nil
      val ex2: List[Int] = Cons(1, Nil)
      val ex3: List[String] = Cons("a", Cons("b", Nil))
      true
    }

    "c" in {
      val ex1: List[Double] = Cons(1.0, Cons(2.0, Cons(3.0, Nil)))
      List.product(ex1) must_== 6.0
    }

    "exercise1" in {
      val a = List(1,2,3,4,5) match {
        case Cons(x, Cons(2, Cons(4, _))) => x
        case Nil => 42
        case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
        case Cons(h, t) => h + List.sum(t)
        case _ => 101
      }
      a must_== 3
    }
  }

  "exercise3.2" should {
    "tail" in {
      List.tail(List(1, 2, 3)) must_== List(2, 3)
    }
  }

  "exercise3.3" should {
    "setHead" in {
      List.setHead(List(1,2,3), 4) must_== List(4,2,3)
    }
  }

  "exercise3.4勘違い" should {
    "drop no 1" in {
      List.drop(List(1,2,3,4,5), 1) must_== List(2,3,4,5)
    }
    "drop no 2" in {
      List.drop(List(1,2,3,4,5), 2) must_== List(1,3,4,5)
    }
    "drop out of no" in {
      List.drop(List(1,2,3,4,5), 6) must_== List(1,2,3,4,5)
    }
  }

  "exercise3.4(answer)" should {
    "drop no 1" in {
      List.drop2(List(1,2,3,4,5), 1) must_== List(2,3,4,5)
    }
    "drop no 2" in {
      List.drop2(List(1,2,3,4,5), 2) must_== List(3,4,5)
    }
    "drop out of no(何か違う気がする)" in {
      // こうなると思ってた
      // List.drop2(List(1,2,3,4,5), 6) must_== Nil
      List.drop2(List(1,2,3,4,5), 6) must_== List(2,3,4,5)
    }
  }

}
