package example

import munit.FunSuite

class ExampleSpec extends FunSuite {

  val mainFunction = new Main()

  test("challenge test 1") {
    assertEquals(mainFunction.challengeFunction(""), true)
  }
}
