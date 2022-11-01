package example

import munit.FunSuite

class ExampleSpec extends FunSuite {

  val mainFunction: Main.type = Main

  test("challenge test 1") {
    assert(mainFunction.challengeFunction("10,5") == "01010")
  }

  test("challenge test 2") {
    assert(mainFunction.challengeFunction("7,10") == "0000000111")
  }

  test("challenge test 3") {
    assert(mainFunction.challengeFunction("128,5") == "00000")
  }

  test("challenge test 4") {
    assert(mainFunction.challengeFunction("127,7") == "1111111")
  }

  test("challenge test 5") {
    assert(mainFunction.challengeFunction("0,7") == "0000000")
  }
}
