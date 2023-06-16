package example

import munit.FunSuite

class ExampleSpec extends FunSuite {

  val mainFunction = new Main()

  private def parseEquation(input: String): Double = {
    input
      .split(" ")
      .foldLeft((Seq[String](), Seq[Double]())) {
        case ((ops: Seq[String], vals: Seq[Double]), value: String) =>
          value match {
            case "(" => (ops, vals)
            case "+" => (value +: ops, vals)
            case "-" => (value +: ops, vals)
            case "*" => (value +: ops, vals)
            case "/" => (value +: ops, vals)
            case ")" =>
              (ops.tail, (ops.head match {
                case "+" => vals(1) + vals.head
                case "-" => vals(1) - vals.head
                case "*" => vals(1) * vals.head
                case "/" => vals(1) / vals.head
              }) +: vals.drop(2))
            case item => (ops, item.toDouble +: vals)
          }
      }
      ._2
      .head
  }

  private def checkNumberUsage(setup: Seq[Int], output: String): Boolean = {
    val parsedOutput = output.split(" ").toSeq.filterNot("()+-*/".contains(_)).map {
      _.toDouble.toInt
    }

    val outputCounts = parsedOutput.distinct.map(x => (x, parsedOutput.count(_ == x)))

    outputCounts.forall {
      case (num, count) =>
        val allowedNumber = setup.count(_ == num)
        count <= allowedNumber
    }
  }

  private def checkOperationUsage(setup: Seq[String], output: String): Boolean = {
    val parsedSetup = output.split(" ").toSeq.filter("+-*/".contains(_))

    val outputCounts = parsedSetup.distinct.map(x => (x, parsedSetup.count(_ == x)))

    outputCounts.forall {
      case (num, count) =>
        val allowedNumber = setup.count(_ == num)
        count <= allowedNumber
    }
  }

  private def testFunction(input: Seq[Int], operations: Seq[String], target: Int, possible: Boolean) = {
    val output = mainFunction.challengeFunction(input, operations, target)
    if (possible) {
      val parsedOutput = parseEquation(output.get)
      assertEquals(parsedOutput, target.toDouble)
      val checkedNumbers = checkNumberUsage(input, output.get)
      assertEquals(checkedNumbers, true)
      val checkOperations = checkOperationUsage(operations, output.get)
      assertEquals(checkOperations, true)
    } else {
      assertEquals(output, None)
    }
  }

  test("challenge test 1") {
    val input = Seq(3, 3, 9, 10, 50, 75)
    val ops = Seq("+", "+", "-", "*", "/")
    val target = 783

    testFunction(input, ops, target, true)
  }

  test("challenge test 2") {
    val input = Seq(3, 3, 9, 10, 50, 75)
    val ops = Seq("+", "-", "*", "/")
    val target = 783

    testFunction(input, ops, target, false)
  }

  test("challenge test 3") {
    val input = Seq(3, 3, 9, 10, 50, 75)
    val ops = Seq("+", "-", "*", "/", "/")
    val target = 783

    testFunction(input, ops, target, true)
  }

  test("challenge test 4") {
    val input = Seq(3, 3, 9, 10, 50, 75)
    val ops = Seq("+", "-", "*", "/")
    val target = 1970

    testFunction(input, ops, target, false)
  }

  test("challenge test 5") {
    val input = Seq(10, 10, 10, 10)
    val ops = Seq("+", "+", "*", "*")
    val target = 400

    testFunction(input, ops, target, true)
  }

  test("challenge test 6") {
    val input = Seq(10, 10, 10, 10)
    val ops = Seq("+", "-", "*", "/")
    val target = 400

    testFunction(input, ops, target, false)
  }

  test("challenge test 7") {
    val input = Seq(10, 40, 50)
    val ops = Seq("-", "-")
    val target = 20

    testFunction(input, ops, target, true)
  }

  test("challenge test Performance 1") {
    val input = Seq(6, 7, 8, 8, 11, 13, 16, 17, 19, 27, 80)

    val ops = Seq("+", "+", "-", "/", "/", "*", "*", "*")
    testFunction(input, ops, 1350, true)
  }
}
