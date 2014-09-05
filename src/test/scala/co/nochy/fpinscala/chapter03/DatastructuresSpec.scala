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
  }

}
