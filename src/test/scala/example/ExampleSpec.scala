package example

import munit.FunSuite

class ExampleSpec extends FunSuite {

  val mainFunction: Main.type = Main

  test("challenge test 1") {
    assertEquals(mainFunction.challengeFunction(""), true)
  }
}
