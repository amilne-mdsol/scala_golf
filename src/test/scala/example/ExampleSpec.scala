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

  test("challenge test 4") {
    assert(mainFunction.challengeFunction(Seq(90, 90, 90, 90, 5, 90, 90, 90, 90)) == Seq())
  }

  test("challenge test 5") {
    assert(mainFunction.challengeFunction(Seq(10, 10, 10, 10, 20, 20, 10, 10, 10)) == Seq((10, 20, 20), (20, 20, 10)))
  }

  test("challenge test 6") {
    assert(
      mainFunction
        .challengeFunction(Seq(10, 10, 10, 10, 20, 20, 20, 10, 10)) == Seq((10, 20, 20), (20, 20, 20), (20, 20, 10)),
    )
  }

  test("challenge test 7") {
    assert(mainFunction.challengeFunction(Seq(10, 10, 10, 10, 20, 20, 30, 10, 10)) == Seq((20, 30, 10)))
  }

  test("challenge test 8") {
    assert(mainFunction.challengeFunction(Seq(10, 10, 10, 10, 20, 21, 20, 10, 10)) == Seq((20, 21, 20)))
  }

  test("challenge test 9") {
    assert(
      mainFunction
        .challengeFunction(Seq(10, 20, 20, 0, 30, 31, 30, 30, 10)) == Seq((10, 20, 20), (20, 20, 0), (30, 31, 30)),
    )
  }

  test("challenge test 10") {
    assert(
      mainFunction
        .challengeFunction(Seq(0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0)) == Seq(
        (0, 1, 1),
        (1, 1, 1),
        (1, 1, 1),
        (1, 1, 1),
        (1, 1, 1),
        (1, 1, 1),
        (1, 1, 1),
        (1, 1, 1),
        (1, 1, 0),
      ),
    )
  }

  test("challenge test 11") {
    assert(
      mainFunction
        .challengeFunction(Seq(0, 1, 1, 1, 1, 2, 2, 2, 2, 1, 1, 1, 1, 1, 0)) == Seq(
        (1, 2, 2),
        (2, 2, 2),
        (2, 2, 2),
        (2, 2, 1),
      ),
    )
  }
}
