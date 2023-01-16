package example

import munit.FunSuite

class ExampleSpec extends FunSuite {

  val mainFunction: Main.type = Main

  test("challenge test 1") {
    assertEquals(mainFunction.challengeFunction(1), "One")
  }

  test("challenge test 2") {
    assertEquals(mainFunction.challengeFunction(2), "Two")
  }

  test("challenge test 3") {
    assertEquals(mainFunction.challengeFunction(95716), "Ninety five thousand, seven hundred and sixteen")
  }

  test("challenge test 4") {
    assertEquals(
      mainFunction.challengeFunction(1111111111),
      "One billion, one hundred and eleven million, one hundred and eleven thousand, one hundred and eleven",
    )
  }

  test("challenge test 5") {
    assertEquals(mainFunction.challengeFunction(9000), "Nine thousand")
  }

  test("challenge test 6") {
    assertEquals(
      mainFunction.challengeFunction(7361529),
      "Seven million, three hundred and sixty one thousand, five hundred and twenty nine",
    )
  }

  test("challenge test 7") {
    assertEquals(mainFunction.challengeFunction(1000), "One thousand")
  }

  test("challenge test 8") {
    assertEquals(mainFunction.challengeFunction(1000000), "One million")
  }

  test("challenge test 9") {
    assertEquals(mainFunction.challengeFunction(1000000000), "One billion")
  }

  test("challenge test 10") {
    assertEquals(
      mainFunction.challengeFunction(1812613414),
      "One billion, eight hundred and twelve million, six hundred and thirteen thousand, four hundred and fourteen",
    )
  }

  test("challenge test 11") {
    assertEquals(
      mainFunction.challengeFunction(1015017918),
      "One billion, fifteen million, seventeen thousand, nine hundred and eighteen",
    )
  }

  test("challenge test 12") {
    assertEquals(
      mainFunction.challengeFunction(1019035949),
      "One billion, nineteen million, thirty five thousand, nine hundred and forty nine",
    )
  }

  test("challenge test 13") {
    assertEquals(
      mainFunction.challengeFunction(1059070989),
      "One billion, fifty nine million, seventy thousand, nine hundred and eighty nine",
    )
  }

  test("challenge test 14") {
    assertEquals(
      mainFunction.challengeFunction(100),
      "One hundred",
    )
  }

  test("challenge test 15") {
    assertEquals(
      mainFunction.challengeFunction(500),
      "Five hundred",
    )
  }
}
