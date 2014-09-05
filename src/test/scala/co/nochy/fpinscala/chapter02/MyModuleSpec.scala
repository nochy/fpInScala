package co.nochy.fpinscala.chapter02

import org.specs2.mutable.Specification


class MyModuleSpec extends Specification {

  "FibonacciNumber.findFirst" should {
    "array(7, 9, 13) x = 9" in {
      MyModule.findFirst(Array(7, 9, 13), 9) must_== 1
    }
  }

  "FibonacciNumber.fib" should {

    "when args 0" in {
      MyModule.fib(0) must_== 0
    }

    "when args 1" in {
      MyModule.fib(1) must_== 1
    }

    "when args 2" in {
      MyModule.fib(2) must_== 1
    }

    "when args 3" in {
      MyModule.fib(3) must_== 2
    }

    "when args 4" in {
      MyModule.fib(4) must_== 3
    }

    "when args 5" in {
      MyModule.fib(5) must_== 5
    }

    "when args 6" in {
      MyModule.fib(6) must_== 8
    }

    "when args 7" in {
      MyModule.fib(7) must_== 13
    }

    "when args 8" in {
      MyModule.fib(8) must_== 21
    }

    "when args 9" in {
      MyModule.fib(9) must_== 34
    }
  }

  "partial" should {
    "test" in {
      val p = MyModule.partial1(1, (a: Int, b: Int) => a + b)
      p(2) must_== 3
    }
  }

  "curried" should {
    "test" in {
      val curriedAdd = MyModule.curry((a: Int, b: Int) => a + b)
      curriedAdd(5)(2) must_== 7
    }
  }

  "uncurried" should {
    "test" in {
      val funcAdd = (a: Int, b: Int) => a + b
      funcAdd(5, 2) must_== 7
      val curriedAdd = MyModule.curry(funcAdd)
      curriedAdd(5)(2) must_== 7
      val uncurriedAdd = MyModule.uncurry(curriedAdd)
      uncurriedAdd(5, 2) must_== 7
    }
  }
}
