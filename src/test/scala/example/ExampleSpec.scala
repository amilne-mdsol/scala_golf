package example

import munit.FunSuite

class ExampleSpec extends FunSuite {

  val mainFunction: Main.type = Main

  test("challenge test 1") {
    assertEquals(
      mainFunction.challengeFunction(2, 2),
      "┌────┬────┐\n" +
        "│    │    │\n" +
        "├────┼────┤\n" +
        "│    │    │\n" +
        "└────┴────┘",
    )
  }

  test("challenge test 2") {
    assertEquals(
      mainFunction.challengeFunction(2, 3),
      "┌────┬────┐\n" +
        "│    │    │\n" +
        "├────┼────┤\n" +
        "│    │    │\n" +
        "├────┼────┤\n" +
        "│    │    │\n" +
        "└────┴────┘",
    )
  }

  test("challenge test 3") {
    assertEquals(
      mainFunction.challengeFunction(1, 1),
      "┌────┐\n" +
        "│    │\n" +
        "└────┘",
    )
  }

  test("challenge test 4") {
    assertEquals(
      mainFunction.challengeFunction(1, 5),
      "┌────┐\n" +
        "│    │\n" +
        "├────┤\n" +
        "│    │\n" +
        "├────┤\n" +
        "│    │\n" +
        "├────┤\n" +
        "│    │\n" +
        "├────┤\n" +
        "│    │\n" +
        "└────┘",
    )
  }

  test("challenge test 5") {
    assertEquals(
      mainFunction.challengeFunction(5, 1),
      "┌────┬────┬────┬────┬────┐\n" +
        "│    │    │    │    │    │\n" +
        "└────┴────┴────┴────┴────┘",
    )
  }

  test("challenge test 6") {
    assertEquals(
      mainFunction.challengeFunction(5, 5),
      "┌────┬────┬────┬────┬────┐\n" +
        "│    │    │    │    │    │\n" +
        "├────┼────┼────┼────┼────┤\n" +
        "│    │    │    │    │    │\n" +
        "├────┼────┼────┼────┼────┤\n" +
        "│    │    │    │    │    │\n" +
        "├────┼────┼────┼────┼────┤\n" +
        "│    │    │    │    │    │\n" +
        "├────┼────┼────┼────┼────┤\n" +
        "│    │    │    │    │    │\n" +
        "└────┴────┴────┴────┴────┘",
    )
  }

  test("challenge test 7") {
    assertEquals(
      mainFunction.challengeFunction(2, 1),
      "┌────┬────┐\n" +
        "│    │    │\n" +
        "└────┴────┘",
    )
  }

  test("challenge test 8") {
    assertEquals(
      mainFunction.challengeFunction(1, 2),
      "┌────┐\n" +
        "│    │\n" +
        "├────┤\n" +
        "│    │\n" +
        "└────┘",
    )
  }

  test("challenge test 9") {
    assertEquals(
      mainFunction.challengeFunction(0, 0),
      "",
    )
  }

  test("challenge test 10") {
    assertEquals(
      mainFunction.challengeFunction(0, 9),
      "",
    )
  }

  test("challenge test 11") {
    assertEquals(
      mainFunction.challengeFunction(9, 0),
      "",
    )
  }

  test("challenge test 12") {
    assertEquals(
      mainFunction.challengeFunction(1, 11),
      "┌────┐\n" +
        "│    │\n" +
        "├────┤\n" +
        "│    │\n" +
        "├────┤\n" +
        "│    │\n" +
        "├────┤\n" +
        "│    │\n" +
        "├────┤\n" +
        "│    │\n" +
        "├────┤\n" +
        "│    │\n" +
        "├────┤\n" +
        "│    │\n" +
        "├────┤\n" +
        "│    │\n" +
        "├────┤\n" +
        "│    │\n" +
        "├────┤\n" +
        "│    │\n" +
        "├────┤\n" +
        "│    │\n" +
        "└────┘",
    )
  }

  test("challenge test 13") {
    assertEquals(
      mainFunction.challengeFunction(11, 1),
      "┌────┬────┬────┬────┬────┬────┬────┬────┬────┬────┬────┐\n" +
        "│    │    │    │    │    │    │    │    │    │    │    │\n" +
        "└────┴────┴────┴────┴────┴────┴────┴────┴────┴────┴────┘",
    )
  }
}
