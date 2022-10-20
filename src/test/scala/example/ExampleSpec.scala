package example

import munit.FunSuite

class ExampleSpec extends FunSuite {

  val mainFunction: Main.type = Main

  test("challenge test 1") {
    assert(mainFunction.challengeFunction("azzbmmzdd"))
  }

  test("challenge test 2") {
    assert(mainFunction.challengeFunction("azzbmmzziee"))
  }

  test("challenge test 3") {
    assert(!mainFunction.challengeFunction("aabbrr"))
  }

  test("challenge test 4") {
    assert(mainFunction.challengeFunction(""))
  }

  test("challenge test 5") {
    assert(mainFunction.challengeFunction("aaaaaaaaaaaaaaaaa111aaaaaaaa985428745aaaa"))
  }

  test("challenge test 6") {
    assert(!mainFunction.challengeFunction("fffffffffffffzzzyybbbbbbbbaaabbbbbaa111aaaaaaaa985428745aaaa"))
  }

  test("challenge test 7") {
    assert(mainFunction.challengeFunction("IlIlIlIlIl1iIlI1Il!l!liI1IL"))
  }

  test("challenge test 8") {
    assert(mainFunction.challengeFunction("zzzyyxxccbbaabbccxxyyzzzzzzz"))
  }

  test("challenge test 9") {
    assert(!mainFunction.challengeFunction("0OO00OO00OO00QQ@@OO00"))
  }
}
