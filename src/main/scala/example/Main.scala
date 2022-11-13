package example

object Main {

  /*
    Challenge 8
    Keyboard distance

    Given a string representing array of characters, give the minimum overall distance a finger would have to travel to type in the given word.

    Note: There can be duplicate letters in the keyboard layout.
    You will need to find the shortest total distance taking these multiple options into consideration.

    This challenge has two ways to win

    1 - Shortest code solution. Use the fewest non-space characters possible to fulfill all the test scenarios.
    2 - Fastest code solution. Write the most clock-cycle efficient method for solving this problem.

    Notes:
    The function declaration is not to be altered. These characters will not be added to the character count.
    Round the result to a 1 decimal place Double
    Keys are 1 unit square and there is no gap between keys.
    If the input word uses a letter that isn't in the keyboard then return -1.0 for the result
    No Variables, this is Scala, we use Values.

    Example and explanation:

    challenge test 1
    assertEquals(mainFunction.challengeFunction("""qwertyuiop
      |asdfghjkl
      |zxcvbnm""".stripMargin, "inputword"), 28.7)

    This means that the keyboard would be like this:
    qwertyuiop
    asdfghjkl-
    zxcvbnm---

    Which is just a slightly shifted version of the regular English language keyboard

    Imagine you are typing with just one finger going from letter to letter in a perfectly straight line

    If the word was "it"
    You would start with your finger above the "i"
    Move to the left 3 buttons to the "t" letter
    This is a distance of 3, so the output from this keyboard for the word "it" would be 3.0

    Typing the word "inputword" is a bit longer, but just the same calculation for each letter.

    Moving from letter to letter in a big zigzag pattern for the whole word results in a distance of 28.7 (rounded to 1dp)

    Here is the input keyboard with all the unused letters removed for display purposes.
    -w-rt-uiop
    --d-------
    -----n----

    i -> n = down 2 left 2 (sqrt(2^2 + 2^2)) approx 2.8
    n -> p = up 2 right 4 approx 4.5
    p -> u = left 3
    u -> t = left 2
    t -> w = left 3
    w -> o = right 7
    o -> r = left 5
    r -> d = down 1 left 1 approx 1.4

    Adding all these together equal 28.7

    In the second test there is another "w" letter that results in a shorter distance overall,
    so the second test has a very similar keyboard design (with the extra w at the end) but a shorter distance of 24.6

   */

  def challengeFunction(k: String, w: String): Double = {
    0.0d
  }

}
