package example

import munit.FunSuite

class ExampleSpec extends FunSuite {

  val mainFunction: Main.type = Main

  test("challenge test easy 1") {
    assertEquals(mainFunction.challengeFunction(
      """qwertyuiop
        |asdfghjkl
        |zxcvbnm""".stripMargin, "i"), 0.0)
  }

  test("challenge test easy 2") {
    assertEquals(mainFunction.challengeFunction(
      """qwertyuiop
        |asdfghjkl
        |zxcvbnm""".stripMargin, "qp"), 9.0)
  }

  test("challenge test easy 3") {
    assertEquals(mainFunction.challengeFunction(
      """qwertyuiop
        |asdfghjkl
        |zxcvbnm""".stripMargin, "in"), 2.8)
  }

  test("challenge test easy 4") {
    assertEquals(mainFunction.challengeFunction(
      """qwertyuiop
        |asdfghjkl
        |zxcvbnm""".stripMargin, "IN"), 2.8)
  }

  test("challenge test easy 5") {
    assertEquals(mainFunction.challengeFunction(
      """qwertyuiop
        |asdfghjkl
        |zxcvbnm""".stripMargin, "ini"), 5.7)
  }

  test("challenge test easy 6") {
    assertEquals(mainFunction.challengeFunction(
      """qwertyuiop
        |asdfghjkl!
        |zxcvbnm""".stripMargin, "ini!"), 7.9)
  }

  test("challenge test 1") {
    assertEquals(mainFunction.challengeFunction("""qwertyuiop
        |asdfghjkl
        |zxcvbnm""".stripMargin, "inputword"), 28.7)
  }

  test("challenge test 2") {
    assertEquals(mainFunction.challengeFunction("""qwertyuiop
        |asdfghjkl
        |zxcvbnmw""".stripMargin, "inputword"), 24.6)
  }

  test("challenge test 3") {
    assertEquals(mainFunction.challengeFunction("""qwertyuop
        |asdfghjkl
        |zxcvbnmw""".stripMargin, "InputWord"), -1.0)
  }

  test("challenge test 4") {
    assertEquals(mainFunction.challengeFunction(
      """qwertyuiop
        |asdfghjkl
        |
        |zxcvbnmw""".stripMargin, "inputword"), 27.4)
  }

  test("challenge test 5") {
    assertEquals(mainFunction.challengeFunction(
      """a
        |b
        |bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbz""".stripMargin, "az"), 32.1)
  }

  test("challenge test 6") {
    assertEquals(mainFunction.challengeFunction(
      """a
        |b
        |bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbz""".stripMargin, "abz"), 32.1)
  }

  test("challenge test 7") {
    assertEquals(mainFunction.challengeFunction(
      """a
        |b
        |bbbbbbbbbbz""".stripMargin, "baaabbzzzbbb"), 12.2)
  }

  test("challenge test 8") {
    assertEquals(
      mainFunction.challengeFunction(
        """zazzzzzzzzzzzzzzzzzya
          |bzzzzzzzzzzzzzzzbzzzz
          |zazzzzzzzzzzzzzzzzzxa""".stripMargin,
        "yabax",
      ),
      10.2,
    )
  }

  test("challenge test 9") {
    assertEquals(
      mainFunction.challengeFunction(
        """a                            a
          |b                         b
          |cd                           cd               z""".stripMargin,
        "abcdabcdz",
      ),
      32.9,
    )
  }

  test("challenge test 10") {
    assertEquals(
      mainFunction.challengeFunction(
        """abbbbbbbbbbbbbbbbbz
          |b
          |abbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbz""".stripMargin,
        "baaabbzzzbbbaaabbzzzbbaaabbzzzbbaaabbzzzbbaaabbzzzbbaaabbzzzbb",
      ),
      200.0,
    )
  }
}
