package example

import org.openjdk.jmh.annotations.{Benchmark, BenchmarkMode, Fork, Mode, OutputTimeUnit, Scope, State}
import org.openjdk.jmh.infra.Blackhole

import java.util.concurrent.TimeUnit

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@State(Scope.Thread)
@Fork(1)
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

  /*@Benchmark
  def testBenchmarkFunction(bh: Blackhole): Unit = {
    val x = Int.MaxValue
    val y = Int.MaxValue
    bh.consume((0 to 10000).map { _ =>
      challengeFunction(x, y)
      0
    })
  }*/

  val c = " ░▒▓█│║─═┐┘└┌┴┬├┤┼╕╛╘╒╧╤╞╡╪╗╝╚╔╩╦╠╣╬╖╜╙╓╨╥╟╢╫"

  def challengeFunction(x: Int, y: Int): String = {
    //challengeFunctionAlecFast(x, y)
    //challengeFunctionAlecSmall(x, y)

    //challengeFunctionLukeShort(x, y)

    challengeFunctionBenFast(x, y)
  }

  def challengeFunctionAlecFast(x: Int, y: Int): String = {

    val c1 = Seq(
      Seq("┌────", "┬────", "┐\n", "├────", "┼────", "┤\n", "└────", "┴────", "┘\n"),
      Seq("╒════", "╤════", "╕\n", "╞════", "╪════", "╡\n", "╘════", "╧════", "╛\n"),
      Seq("╓────", "╥────", "╖\n", "╟────", "╫────", "╢\n", "╙────", "╨────", "╜\n"),
      Seq("╔════", "╦════", "╗\n", "╠════", "╬════", "╣\n", "╚════", "╩════", "╝\n"),
    )

    val c2 = "│║"

    val c3 = Seq("    ", "░░░░", "▒▒▒▒", "▓▓▓▓", "████")

    val xDigits = x.toBinaryString.map(_.toInt - 48)
    val yDigits = y.toBinaryString.map(_.toInt - 48)
    val xLimit = xDigits.length - 1
    val yLimit = yDigits.length - 1

    if (y > 1 && x > 1) {
      (for {
        yRow <- 0 to yLimit
        yDigit = yDigits(yRow)
        a = if (yRow == 0) 0 else if (yRow < yLimit) 3 else 6
        xRow <- 0 to xLimit
        pieceType = if (xRow == 0) a else if (xRow < xLimit) a + 1 else a + 2
      } yield {
        c1(xDigits(xRow) * 2 + yDigit)(pieceType) +
          (if (xRow < xLimit) ""
          else if (yRow < yLimit)
            (0 to xLimit).map { xCol =>
              c2(xDigits(xCol)) + (if (xCol < xLimit)
                c3(xDigits(xCol) + xDigits(xCol + 1) + yDigit + yDigits(yRow + 1))
              else "\n")
            }.mkString
          else "")
      }).mkString.dropRight(1)
    } else {
      ""
    }
  }

  def challengeFunctionAlecSmall(x: Int, y: Int): String = {
    val Seq(d, e) = Seq(x, y).map(_.toBinaryString.map(_.toInt - 48))
    val q = e.length - 1
    val r = d.length - 1

    (for {
      n <- 0 to q
      t = e(n)
      a = if (n == 0) 27 else if (n < q) 61 else 19
      m <- 0 to r
      if y > 1 & x > 1
    } yield {
      c(9 + m / r * a % 9 + (1 - m / r) * a / 9 + Math.min(1, m % r) * 2 + (3 - 2 * t) * 9 * d(m) + 9 * t) +
        (if (m < r) ("" + c(7 + t)) * 4
        else if (n < q)
          "\n" + (0 to r).map { k =>
            c(5 + d(k)) + (if (k < r) ("" + c(d(k) + d(k + 1) + t + e(n + 1))) * 4 else "\n")
          }.mkString
        else "")
    }).mkString
  } //358

  def challengeFunctionLukeShort(x: Int, y: Int): String = {
    (
      for {
        _ <- "l"
        if x > 1 & y > 1
        e = "┌┬┐├┼┤└┴┘╓╥╖╟╫╢╙╨╜╒╤╕╞╪╡╘╧╛╔╦╗╠╬╣╚╩╝"
        f = y.toBinaryString map (i => x.toBinaryString map ("" + i + _))
        (a, i) <- f.zipWithIndex
        d = i < f.size - 1
        l +: m :+ n = a map (9 * Integer.parseInt(_, 2) + (if (d) 3 * Math.min(i, 1) else 6))
      } yield (
        e(l) +: m.map(t => e(t + 1)) :+ e(n + 2) mkString ("" + c(if (l < 19) 7 else 8)) * 4
        ) + (
        if (d)
          "\n" + (
            (0 to a.size - 2) map (
              t => c(5 + a(t)(1) % 2) + (a zip f(i + 1).tail map (q => ("" + c((q._1 + q._2) count (_ == '1'))) * 4))(t),
              )
            ).mkString + c(5 + x % 2)
        else ""
        )
      ) mkString "\n"
  } // 431

  def challengeFunctionBenFast(x: Int, y: Int): String = {

    if (x < 2) return ""
    if (y < 2) return ""

    val binaryX = x.toBinaryString.toCharArray
    val binaryY = y.toBinaryString.toCharArray
    val xl = binaryX.length
    val yl = binaryY.length
    val xl_ = xl - 1
    val yl_ = yl - 1
    val xf = xl_ * 5 + 1
    val yf = yl_ * 2 + 1
    val l = xf + 1

    val finalArray = new Array[Char](l * yf)
    val iLim = xl_ * 5
    val jLim = yl_ * 2

    def getCharPainful(i: Int, j: Int): Char = {
      if (i == xf) '\n'
      else {
        val iBin = binaryX(i / 5)
        val jBin = binaryY(j / 2)

        if (i == 0) {
          if (j == 0) {
            if (iBin == '0' && jBin == '0') '┌'
            else if (iBin == '0' && jBin == '1') '╒'
            else if (iBin == '1' && jBin == '0') '╓'
            else '╔'
          } else if (j == jLim) {
            if (iBin == '0' && jBin == '0') '└'
            else if (iBin == '0' && jBin == '1') '╘'
            else if (iBin == '1' && jBin == '0') '╙'
            else '╚'
          } else if (j % 2 == 0) {
            if (iBin == '0' && jBin == '0') '├'
            else if (iBin == '0' && jBin == '1') '╞'
            else if (iBin == '1' && jBin == '0') '╟'
            else '╠'
          } else {
            if (iBin == '0') '│'
            else '║'
          }
        } else if (i == iLim) {
          if (j == 0) {
            if (iBin == '0' && jBin == '0') '┐'
            else if (iBin == '0' && jBin == '1') '╕'
            else if (iBin == '1' && jBin == '0') '╖'
            else '╗'
          } else if (j == jLim) {
            if (iBin == '0' && jBin == '0') '┘'
            else if (iBin == '0' && jBin == '1') '╛'
            else if (iBin == '1' && jBin == '0') '╜'
            else '╝'
          } else if (j % 2 == 0) {
            if (iBin == '0' && jBin == '0') '┤'
            else if (iBin == '0' && jBin == '1') '╡'
            else if (iBin == '1' && jBin == '0') '╢'
            else '╣'
          } else {
            if (iBin == '0') '│'
            else '║'
          }
        } else if (i % 5 == 0) {
          if (j == 0) {
            if (iBin == '0' && jBin == '0') '┬'
            else if (iBin == '0' && jBin == '1') '╤'
            else if (iBin == '1' && jBin == '0') '╥'
            else '╦'
          } else if (j == jLim) {
            if (iBin == '0' && jBin == '0') '┴'
            else if (iBin == '0' && jBin == '1') '╧'
            else if (iBin == '1' && jBin == '0') '╨'
            else '╩'
          } else if (j % 2 == 0) {
            if (iBin == '0' && jBin == '0') '┼'
            else if (iBin == '0' && jBin == '1') '╪'
            else if (iBin == '1' && jBin == '0') '╫'
            else '╬'
          } else {
            if (iBin == '0') '│'
            else '║'
          }
        } else if (j % 2 == 0) {
          if (jBin == '0') '─'
          else '═'
        } else {
          val iBin2 = binaryX(i / 5 + 1)
          val jBin2 = binaryY(j / 2 + 1)
          val sum = iBin + iBin2 + jBin + jBin2
          if (sum == 196) '█'
          else if (sum == 195) '▓'
          else if (sum == 194) '▒'
          else if (sum == 193) '░'
          else ' '
        }
      }
    }

    import scala.concurrent.ExecutionContext.Implicits.global

    def run(lowInc: Int, upExc: Int): scala.concurrent.Future[Unit] = scala.concurrent.Future {
      var j = lowInc
      while (j < upExc) {
        var i = 0
        while (i < l) {
          finalArray(i + j * l) = getCharPainful(i, j)
          i = i + 1
        }
        j = j + 1
      }
    }

    val PARALLELISM = 4

    val threads = (0 until PARALLELISM)
      .map(t => ((t * yf) / PARALLELISM, ((t + 1) * yf) / PARALLELISM))
      .map { case (min, max) => run(min, max) }

    val collectedThreads = scala.concurrent.Future.sequence(threads)

    scala.concurrent.Await.result(collectedThreads, scala.concurrent.duration.Duration.Inf)

    val result = new String(finalArray.dropRight(1))

    result
  }
}
