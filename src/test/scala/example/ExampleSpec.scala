package example

import munit.FunSuite

class ExampleSpec extends FunSuite {

  val mainFunction: Main.type = Main

  test("challenge test 1") {
    assert(mainFunction.challengeFunction(Seq(1, 5, 1, 6, 7, 8, 7, 6, 8)) == Seq((1, 5, 1), (7, 8, 7)))
  }

  test("challenge test 2") {
    assert(mainFunction.challengeFunction(Seq(1, 2, 3, 4, 5, 6, 7, 8, 9, 1)) == Seq((8, 9, 1)))
  }

  test("challenge test 3") {
    assert(mainFunction.challengeFunction(Seq(0, 0, 0, 0, 5, 0, 0, 0, 0)) == Seq((0, 5, 0)))
  }
}
