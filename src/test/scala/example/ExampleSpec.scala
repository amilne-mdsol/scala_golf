package example

import munit.FunSuite

class ExampleSpec extends FunSuite {

  val mainFunction: Main.type = Main

  test("challenge test 1") {
    assertEquals(mainFunction.challengeFunction(1, "abracadabra"), "abracadabra")
  }

  test("challenge test 2") {
    assertEquals(mainFunction.challengeFunction(3, "abracadabra"), "aaabraaacaaadaaabraaa")
  }

  test("challenge test 3") {
    assertEquals(mainFunction.challengeFunction(13, "abracadabra"), "abraaacadaaabra")
  }

  test("challenge test 4") {
    assertEquals(mainFunction.challengeFunction(13, "aaaaaabraaaaaacaaaaaadaaaaaabraaaaaa"), "abraaacadaaabra")
  }

  test("challenge test 5") {
    assertEquals(mainFunction.challengeFunction(3912511, "abracadabra"), "aaabraaaaaaaaacadaabraaaaa")
  }

  test("challenge test 6") {
    assertEquals(mainFunction.challengeFunction(9, "Leeroy Jenkins"), "Leeeeeeeeeroooooooooy Jeeeeeeeeenkiiiiiiiiins")
  }

  test("challenge test 7") {
    assertEquals(mainFunction.challengeFunction(1, "whooooooooooooooooop"), "whop")
  }

  test("challenge test 8") {
    assertEquals(
      mainFunction.challengeFunction(2, "AeiouaeiouaeiouaeiouaeiouaeIouaeiou"),
      "AAeeiioouuaaeeiioouuaaeeiioouuaaeeiioouuaaeeiioouuaaeeIIoouuaaeeiioouu",
    )
  }

  test("challenge test 9") {
    assertEquals(
      mainFunction.challengeFunction(2, "aeiouaeiouaeiouaeiouaeiouaeiouaeiou"),
      "aaeeiioouuaaeeiioouuaaeeiioouuaaeeiioouuaaeeiioouuaaeeiioouuaaeeiioouu",
    )
  }

  test("challenge test 10") {
    assertEquals(
      mainFunction.challengeFunction(33, "aeiouaeiouaeiouaeiouaeiouaeiouaeiou"),
      "aaaeeeiiiooouuuaaaeeeiiiooouuuaaaeeeiiiooouuuaaaeeeiiiooouuuaaaeeeiiiooouuuaaaeeeiiiooouuuaaaeeeiiiooouuu",
    )
  }

  test("challenge test 11") {
    assertEquals(
      mainFunction.challengeFunction(33, "LLLlleEerrrrrroy - !JjJjEnkkInsy"),
      "LLLlleeerrrrrroooy - !JjJjEEEnkkIIInsy",
    )
  }
}
