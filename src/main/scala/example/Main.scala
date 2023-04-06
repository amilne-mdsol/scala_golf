package example

import org.openjdk.jmh.annotations.{Benchmark, BenchmarkMode, Mode, OutputTimeUnit}
import org.openjdk.jmh.infra.Blackhole

import java.util.concurrent.TimeUnit

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
class Main {

  /*
    Challenge 10 - boxes
    Given two integers in, create an output that draws an array of boxes in a string for the output.
    The lines of the boxes are determined by the two numbers in.
    Fill in the sections of boxes according to how many double lines are surrounding each box.

    All relevant ASCII characters are provided in this challenge in an external reference. You don't have to use it.

    The input parameters are positive integers. The first is used for the X axis, the second is used for the Y axis.

    Convert the number for the axis into binary, and then use the 1s and 0s from this number to determine the line thicknesses.

    e.g.
    Number = 25
    Convert to binary: 25 -> 11001
    This would mean double, double, single, single, double

    Let's go over a full example:

    X number = 12
    Y number = 12

    12 in binary is 1100

    So let's build the box first.
     1    1    0    0
    1╔════╦════╤════╕
     ║    ║    │    │
    1╠════╬════╪════╡
     ║    ║    │    │
    0╟────╫────┼────┤
     ║    ║    │    │
    0╙────╨────┴────┘

    You can see that where there is a 1 in the number, the lines of the boxes is doubled.
    If all digits were 1 for both numbers then all the lines would be doubled.

    But, there's one more step needed. We need to fill in those boxes.

    Let's count how many double lines surround each square.

    ╔════╦════╤════╕
    ║ 4  ║ 3  │ 2  │
    ╠════╬════╪════╡
    ║ 3  ║ 2  │ 1  │
    ╟────╫────┼────┤
    ║ 2  ║ 1  │ 0  │
    ╙────╨────┴────┘

    Then we use the existing ASCII filled characters:
    0 double lines = ( )
    1 double lines = (░)
    2 double lines = (▒)
    3 double lines = (▓)
    4 double lines = (█)

    ╔════╦════╤════╕
    ║████║▓▓▓▓│▒▒▒▒│
    ╠════╬════╪════╡
    ║▓▓▓▓║▒▒▒▒│░░░░│
    ╟────╫────┼────┤
    ║▒▒▒▒║░░░░│    │
    ╙────╨────┴────┘

    This is the expected output when the inputs are 12,12

    To run benchmarking use:
    Jmh/run
    In the sbt shell
   */

  @Benchmark
  def testBenchmarkFunction(bh: Blackhole): Unit = {
    val x = Int.MaxValue
    val y = Int.MaxValue
    bh.consume(challengeFunction(x, y))
  }

  val c = " ░▒▓█│║─═┐┘└┌┴┬├┤┼╕╛╘╒╧╤╞╡╪╗╝╚╔╩╦╠╣╬╖╜╙╓╨╥╟╢╫"

  def challengeFunction(x: Int, y: Int): String = {
    ""
  }
}
