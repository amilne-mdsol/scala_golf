package example

import org.openjdk.jmh.annotations.{Benchmark, BenchmarkMode, Fork, Mode, OutputTimeUnit, Scope, State}
import org.openjdk.jmh.infra.Blackhole

import java.util.concurrent.TimeUnit

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@State(Scope.Thread)
@Fork(2)
class Main {

  /*
  Challenge 11 - Find the equation

  Given a sequence of integers and a sequence of operations, can you reach a specific target number by using the operations on the numbers?

  The output from the function needs to be a string of an equation that gives the number.

  Brackets needs to be around each operation, and there has to be a space between each item in the string.

  You will need a bracket around the entire string output equation. This is all detailed in the example below and in the tests.

  This is like the Countdown numbers game, but also including a list of operations you can use in the mix.

  Note:
  The is no single correct answer for each run. As long as your solution works mathematically to reach the number
  and only uses items from the given input lists, it is an acceptable answer.
  It does not need to be the best or simplest solution, just one that works.
  Floating point numbers can be used in the working out of your solution, but your output should be an exact match of the target.
  Each item in the input lists can only be used once, but there can be repeated items in the input lists.


  Example:
  input numbers: 3, 3, 9, 10, 50, 75
  input operations: +, +, -, *, /
  target = 783

  This number can be done with some of the numbers and operations from the input.

  75 + 3 = 78
  78 * 10 = 780
  780 + 3 = 783

  To put this into the required format:
  "( ( ( 75 + 3 ) * 10 ) + 3 )"
   */

  @Benchmark
  def testBenchmarkFunctionTestFast(bh: Blackhole): Unit = {
    val input = Seq(6, 7, 8, 8, 11, 13, 16, 17, 19, 27, 80)
    val ops = Seq("+", "+", "-", "/", "/", "*", "*")
    val target = 1350
    bh.consume(challengeFunctionAlecFastINVALID2(input, ops, target))
  }

  @Benchmark
  def testBenchmarkFunctionTestInvalid(bh: Blackhole): Unit = {
    val input = Seq(6, 7, 8, 8, 11, 13, 16, 17, 19, 27, 80)
    val ops = Seq("+", "+", "-", "/", "/", "*", "*")
    val target = 1350
    bh.consume(challengeFunctionAlecFastINVALID3(input, ops, target))
  }

  def challengeFunction(i: Seq[Int], o: Seq[String], t: Int): Option[String] = {
    //challengeFunctionAlecFast(i, o, t)
    //challengeFunctionAlecFastINVALID(i, o, t)
    //challengeFunctionAlecFastINVALID22(i, o, t)
    challengeFunctionAlecFastINVALID3(i, o, t)
    //challengeFunctionAlecSmall(i, o, t)
    //challengeFunctionAlecSmall895(i, o, t)

    //challengeFunctionLukeShort(i, o, t)
    //challengeFunctionLukeFast(i,o,t)
  }

  def challengeFunctionAlecSmall895(i: Seq[Int], o: Seq[String], t: Int): Option[String] = {

    abstract class P {
      def c: Double
      def g: Int
      def i(p: Int, n: Int, o: String): P
      def s: String
    }

    case class B(o: String, l: P, r: P) extends P {
      override def c =
        o match {
          case "+" => l.c + r.c
          case "-" => l.c - r.c
          case "*" => l.c * r.c
          case "/" => l.c / r.c
        }

      override def g = l.g + r.g

      override def i(p: Int, n: Int, o: String) =
        if (p > 0) if (p > l.g) this.copy(l = l.i(p, n, o)) else this.copy(r = r.i(p - l.g, n, o))
        else B(o, this, N(n))

      override def s = s"( ${l.s} $o ${r.s} )"
    }

    case class N(v: Int) extends P {
      override def c = v.toDouble
      override def g = 1
      override def i(p: Int, n: Int, o: String) = B(o, this, N(n))
      override def s = "" + v
    }

    def q(n: Seq[(Int, Int)], o: Seq[(String, Int)], p: P, t: Int): Option[P] =
      if (p.c == t.toDouble) Some(p)
      else {
        (for {
          a <- n
          b <- o
          c <- 0 to p.g
        } yield {
          q(n.filterNot(_._2 == a._2), o.filterNot(_._2 == b._2), p.i(c, a._1, b._1), t)
        }).flatten.headOption
      }

    val j = i.zipWithIndex

    j.flatMap { a =>
        q(j.filterNot(_._2 == a._2), o.zipWithIndex, N(a._1), t)
      }
      .headOption
      .map(_.s)
  } // 895

  def challengeFunctionAlecSmall700(i: Seq[Int], o: Seq[String], t: Int): Option[String] = {

    def q(n: Seq[(Int, Int)], o: Seq[(String, Int)], p: Seq[String], t: Int): Option[Seq[String]] =
      if (p.foldLeft(Seq[Double]()) {
            case (v: Seq[Double], value: String) =>
              value match {
                case "+"  => (v(1) + v(0)) +: v.drop(2)
                case "-"  => (v(1) - v(0)) +: v.drop(2)
                case "*"  => (v(1) * v(0)) +: v.drop(2)
                case "/"  => (v(1) / v(0)) +: v.drop(2)
                case item => item.toDouble +: v
              }
          }(0) == t.toDouble) Some(p)
      else {
        (for {
          a <- n
          b <- o
          c <- 1 to p.length
        } yield {
          q(
            n.filterNot(_._2 == a._2),
            o.filterNot(_._2 == b._2),
            p.take(c) ++ Seq("" + a._1, "" + b._1) ++ p.drop(c),
            t,
          )
        }).flatten.headOption
      }

    val j = i.zipWithIndex

    j.flatMap { a =>
        q(j.filterNot(_._2 == a._2), o.zipWithIndex, Seq("" + a._1), t)
      }
      .headOption
      .map(_.foldLeft(Seq[String]()) {
        case (v, k) => if ("+-*/".contains(k)) s"( ${v(1)} $k ${v.head} )" +: v.drop(2) else k +: v
      }(0))
  } // 700

  def challengeFunctionAlecSmall(i: Seq[Int], o: Seq[String], t: Int): Option[String] = {
    def q(n: Seq[(Int, Int)], o: Seq[(String, Int)], p: Seq[String], t: Double): Option[Seq[String]] =
      if (p.foldLeft(Seq[Double]()) {
            case (v, w) =>
              if ("+-*/".contains(w))
                (w match {
                  case "+" => v(1) + v(0)
                  case "-" => v(1) - v(0)
                  case "*" => v(1) * v(0)
                  case _   => v(1) / v(0)
                }) +: v.drop(2)
              else w.toDouble +: v
          }(0) == t) Some(p)
      else {
        (for {
          a <- n
          b <- o
          c <- 1 to p.size
        } yield {
          q(
            n.filterNot(_._2 == a._2),
            o.filterNot(_._2 == b._2),
            p.take(c) ++ Seq("" + a._1, "" + b._1) ++ p.drop(c),
            t,
          )
        }).flatten.headOption
      }

    val j = i.zipWithIndex

    j.flatMap { a =>
        q(j.filterNot(_._2 == a._2), o.zipWithIndex, Seq("" + a._1), t)
      }
      .headOption
      .map(_.foldLeft(Seq[String]()) {
        case (v, k) => if ("+-*/".contains(k)) s"( ${v(1)} $k ${v(0)} )" +: v.drop(2) else k +: v
      }(0))
  } // 634

  def challengeFunctionAlecFast(i: Seq[Int], o: Seq[String], t: Int): Option[String] = {

    abstract class TreePoint {
      def calc(): Double
      def getOpenCount(): Int
      def addItemAtPoint(point: Int, newNumber: Int, newOperation: String): TreePoint
      def toString(): String
    }

    case class TreeBranch(operation: String, left: TreePoint, right: TreePoint) extends TreePoint {
      override def calc(): Double = {
        operation match {
          case "+" => left.calc() + right.calc()
          case "-" => left.calc() - right.calc()
          case "*" => left.calc() * right.calc()
          case "/" => left.calc() / right.calc()
        }
      }

      override def getOpenCount(): Int = { left.getOpenCount() + right.getOpenCount() }

      override def addItemAtPoint(point: Int, newNumber: Int, newOperation: String): TreePoint = {
        if (point == 0) {
          TreeBranch(newOperation, this, TreeNumber(newNumber))
        } else {
          val leftCount = left.getOpenCount()
          if (point > leftCount) {
            this.copy(left = left.addItemAtPoint(point, newNumber, newOperation))
          } else {
            this.copy(right = right.addItemAtPoint(point - leftCount, newNumber, newOperation))
          }
        }
      }

      override def toString(): String = { s"( ${left.toString} $operation ${right.toString} )" }
    }

    case class TreeNumber(value: Int) extends TreePoint {
      override def calc(): Double = value.toDouble

      override def getOpenCount(): Int = { 1 }

      override def addItemAtPoint(point: Int, newNumber: Int, newOperation: String): TreePoint = {
        TreeBranch(newOperation, this, TreeNumber(newNumber))
      }

      override def toString(): String = { "" + value }
    }

    def recursionFunction(
      remainingNumbers: Seq[(Int, Int)],
      remainingOperations: Seq[(String, Int)],
      currentTree: TreePoint,
      target: Int,
    ): Option[TreePoint] = {

      ((currentTree.calc() == target.toDouble), (remainingNumbers.nonEmpty && remainingOperations.nonEmpty)) match {
        case (true, _)      => Some(currentTree)
        case (false, false) => None
        case (false, true) => {
          remainingNumbers.foldLeft(None: Option[TreePoint]) {
            case (carryC: Option[TreePoint], (nextC, nextCPos)) =>
              carryC match {
                case Some(_) => carryC
                case None => {
                  remainingOperations.foldLeft(None: Option[TreePoint]) {
                    case (carryB: Option[TreePoint], (nextB, nextBPos)) =>
                      carryB match {
                        case Some(_) => carryB
                        case None => {
                          (0 to currentTree.getOpenCount()).foldLeft(None: Option[TreePoint]) {
                            case (carry: Option[TreePoint], next) =>
                              carry match {
                                case Some(_) => carry
                                case None => {
                                  val newItem: TreePoint = currentTree.addItemAtPoint(next, nextC, nextB)
                                  recursionFunction(
                                    remainingNumbers.filterNot(_._2 == nextCPos),
                                    remainingOperations.filterNot(_._2 == nextBPos),
                                    newItem,
                                    target,
                                  )
                                }
                              }
                          }
                        }
                      }
                  }
                }
              }
          }
        }
      }
    }

    val iWithIndex = i.zipWithIndex

    iWithIndex.iterator
      .map {
        case (num, numPos) =>
          recursionFunction(
            iWithIndex.filterNot(_._2 == numPos),
            o.zipWithIndex,
            TreeNumber(num),
            t,
          )
      }
      .dropWhile(_.isEmpty)
      .nextOption()
      .flatten
      .map(_.toString())
  }

  def challengeFunctionAlecFastINVALID(i: Seq[Int], o: Seq[String], t: Int): Option[String] = {

    abstract class TreePoint {
      def calc(): Double

      def getOpenCount(): Int

      def addItemAtPoint(point: Int, newNumber: Int, newOperation: String): TreePoint

      def toString(): String
    }

    case class TreeBranch(operation: String, left: TreePoint, right: TreePoint) extends TreePoint {
      override def calc(): Double = {
        operation match {
          case "+" => left.calc() + right.calc()
          case "-" => left.calc() - right.calc()
          case "*" => left.calc() * right.calc()
          case "/" => left.calc() / right.calc()
        }
      }

      override def getOpenCount(): Int = {
        left.getOpenCount() + right.getOpenCount()
      }

      override def addItemAtPoint(point: Int, newNumber: Int, newOperation: String): TreePoint = {
        if (point == 0) {
          TreeBranch(newOperation, this, TreeNumber(newNumber))
        } else {
          val leftCount = left.getOpenCount()
          if (point > leftCount) {
            this.copy(left = left.addItemAtPoint(point, newNumber, newOperation))
          } else {
            this.copy(right = right.addItemAtPoint(point - leftCount, newNumber, newOperation))
          }
        }
      }

      override def toString(): String = {
        s"( ${left.toString} $operation ${right.toString} )"
      }
    }

    case class TreeNumber(value: Int) extends TreePoint {
      override def calc(): Double = value.toDouble

      override def getOpenCount(): Int = {
        1
      }

      override def addItemAtPoint(point: Int, newNumber: Int, newOperation: String): TreePoint = {
        TreeBranch(newOperation, this, TreeNumber(newNumber))
      }

      override def toString(): String = {
        "" + value
      }
    }

    def recursionFunction(
      remainingNumbers: Seq[(Int, Int)],
      remainingOperations: Seq[(String, Int)],
      currentTree: TreePoint,
      target: Double,
    ): Option[TreePoint] = {

      ((currentTree.calc() == target), (remainingNumbers.nonEmpty && remainingOperations.nonEmpty)) match {
        case (true, _)      => Some(currentTree)
        case (false, false) => None
        case (false, true) => {
          remainingNumbers.foldLeft(None: Option[TreePoint]) {
            case (carryC: Option[TreePoint], (nextC, nextCPos)) =>
              val numbersWithoutCurrent = remainingNumbers.filterNot(_._2 == nextCPos)
              carryC match {
                case Some(_) => carryC
                case None => {
                  remainingOperations.foldLeft(None: Option[TreePoint]) {
                    case (carryB: Option[TreePoint], (nextB, nextBPos)) =>
                      val operationsWithoutCurrent = remainingOperations.filterNot(_._2 == nextBPos)
                      carryB match {
                        case Some(_) => carryB
                        case None => {
                          (0 to currentTree.getOpenCount()).foldLeft(None: Option[TreePoint]) {
                            case (carry: Option[TreePoint], next) =>
                              carry match {
                                case Some(_) => carry
                                case None => {
                                  val newItem: TreePoint = currentTree.addItemAtPoint(next, nextC, nextB)
                                  recursionFunction(
                                    numbersWithoutCurrent,
                                    operationsWithoutCurrent,
                                    newItem,
                                    target,
                                  )
                                }
                              }
                          }
                        }
                      }
                  }
                }
              }
          }
        }
      }
    }

    val iWithIndex = i.zipWithIndex

    iWithIndex.iterator
      .map {
        case (num, numPos) =>
          recursionFunction(
            iWithIndex.filterNot(_._2 == numPos),
            o.zipWithIndex,
            TreeNumber(num),
            t.toDouble,
          )
      }
      .dropWhile(_.isEmpty)
      .nextOption()
      .flatten
      .map(_.toString())
  }

  def challengeFunctionAlecFastINVALID2(i: Seq[Int], o: Seq[String], t: Int): Option[String] = {

    abstract class TreePoint {
      def calc(): Double

      def getOpenCount(): Int

      def addItemAtPoint(point: Int, newNumber: Int, newOperation: String): TreePoint

      def toString(): String
    }

    case class TreeBranch(operation: String, left: TreePoint, right: TreePoint) extends TreePoint {
      override def calc(): Double = {
        operation match {
          case "+" => left.calc() + right.calc()
          case "-" => left.calc() - right.calc()
          case "*" => left.calc() * right.calc()
          case "/" => left.calc() / right.calc()
        }
      }

      override def getOpenCount(): Int = {
        left.getOpenCount() + right.getOpenCount()
      }

      override def addItemAtPoint(point: Int, newNumber: Int, newOperation: String): TreePoint = {
        if (point == 0) {
          TreeBranch(newOperation, this, TreeNumber(newNumber))
        } else {
          val leftCount = left.getOpenCount()
          if (point > leftCount) {
            this.copy(left = left.addItemAtPoint(point, newNumber, newOperation))
          } else {
            this.copy(right = right.addItemAtPoint(point - leftCount, newNumber, newOperation))
          }
        }
      }

      override def toString(): String = {
        s"( ${left.toString} $operation ${right.toString} )"
      }
    }

    case class TreeNumber(value: Int) extends TreePoint {
      override def calc(): Double = value.toDouble

      override def getOpenCount(): Int = {
        1
      }

      override def addItemAtPoint(point: Int, newNumber: Int, newOperation: String): TreePoint = {
        TreeBranch(newOperation, this, TreeNumber(newNumber))
      }

      override def toString(): String = {
        "" + value
      }
    }

    def tempRecursionA(
      currentTree: TreePoint,
      nextC: Int,
      nextB: String,
      numbersWithoutCurrent: Seq[(Int, Int)],
      operationsWithoutCurrent: Seq[(String, Int)],
      target: Double,
      runningSeq: Seq[Int],
      testResult: Option[TreePoint],
    ): Option[TreePoint] = {
      (testResult, runningSeq.length) match {
        case (Some(_), _) => testResult
        case (None, 0)    => None
        case (None, _) =>
          val next = runningSeq.head
          val nextRunningSeq = runningSeq.tail
          tempRecursionA(
            currentTree,
            nextC,
            nextB,
            numbersWithoutCurrent,
            operationsWithoutCurrent,
            target,
            nextRunningSeq,
            recursionFunction(
              numbersWithoutCurrent,
              operationsWithoutCurrent,
              currentTree.addItemAtPoint(next, nextC, nextB),
              target,
            ),
          )
      }
    }

    def tempRecursionB(
      currentTree: TreePoint,
      nextC: Int,
      numbersWithoutCurrent: Seq[(Int, Int)],
      remainingOperations: Seq[(String, Int)],
      target: Double,
      runningSeq: Seq[(String, Int)],
      testResult: Option[TreePoint],
    ): Option[TreePoint] = {
      (testResult, runningSeq.length) match {
        case (Some(_), _) => testResult
        case (None, 0)    => None
        case (None, _) =>
          val nextB = runningSeq.head._1
          val nextBPos = runningSeq.head._2
          val runningSeqNext = runningSeq.tail
          val remainingOperationsNext = remainingOperations.filterNot(_._2 == nextBPos)
          tempRecursionB(
            currentTree,
            nextC,
            numbersWithoutCurrent,
            remainingOperations,
            target,
            runningSeqNext,
            tempRecursionA(
              currentTree,
              nextC,
              nextB,
              numbersWithoutCurrent,
              remainingOperationsNext,
              target,
              (0 to currentTree.getOpenCount()),
              None,
            ),
          )
      }
    }

    def tempRecursionC(
      currentTree: TreePoint,
      remainingNumbers: Seq[(Int, Int)],
      remainingOperations: Seq[(String, Int)],
      target: Double,
      runningSeq: Seq[(Int, Int)],
      testResult: Option[TreePoint],
    ): Option[TreePoint] = {
      (testResult, runningSeq.length) match {
        case (Some(_), _) => testResult
        case (None, 0)    => None
        case (None, _) =>
          val nextC = runningSeq.head._1
          val nextCPos = runningSeq.head._2
          val runningSeqNext = runningSeq.tail
          val remainingNumbersNext = remainingNumbers.filterNot(_._2 == nextCPos)
          tempRecursionC(
            currentTree,
            remainingNumbers,
            remainingOperations,
            target,
            runningSeqNext,
            tempRecursionB(
              currentTree,
              nextC,
              remainingNumbersNext,
              remainingOperations,
              target,
              remainingOperations,
              None,
            ),
          )
      }
    }

    def recursionFunction(
      remainingNumbers: Seq[(Int, Int)],
      remainingOperations: Seq[(String, Int)],
      currentTree: TreePoint,
      target: Double,
    ): Option[TreePoint] = {
      ((currentTree.calc() == target), (remainingNumbers.nonEmpty && remainingOperations.nonEmpty)) match {
        case (true, _)      => Some(currentTree)
        case (false, false) => None
        case (false, true) => {
          tempRecursionC(currentTree, remainingNumbers, remainingOperations, target, remainingNumbers, None)
        }
      }
    }

    val iWithIndex = i.zipWithIndex

    iWithIndex.iterator
      .map {
        case (num, numPos) =>
          recursionFunction(
            iWithIndex.filterNot(_._2 == numPos),
            o.zipWithIndex,
            TreeNumber(num),
            t.toDouble,
          )
      }
      .dropWhile(_.isEmpty)
      .nextOption()
      .flatten
      .map(_.toString())
  }

  def challengeFunctionAlecFastINVALID3(i: Seq[Int], o: Seq[String], t: Int): Option[String] = {

    object OperationType extends Enumeration {
      type OperationType = Value

      val Plus, Minus, Multiply, Divide = Value
    }

    abstract class TreePoint {
      def calc(): Double

      def getOpenCount(): Int

      def addItemAtPoint(point: Int, newNumber: Int, newOperation: OperationType.OperationType): TreePoint

      def toString(): String
    }

    case class TreeBranch(operation: OperationType.OperationType, left: TreePoint, right: TreePoint) extends TreePoint {
      override def calc(): Double = {
        operation match {
          case OperationType.Plus     => left.calc() + right.calc()
          case OperationType.Minus    => left.calc() - right.calc()
          case OperationType.Multiply => left.calc() * right.calc()
          case OperationType.Divide   => left.calc() / right.calc()
        }
      }

      override def getOpenCount(): Int = {
        left.getOpenCount() + right.getOpenCount()
      }

      override def addItemAtPoint(point: Int, newNumber: Int, newOperation: OperationType.OperationType): TreePoint = {
        if (point == 0) {
          TreeBranch(newOperation, this, TreeNumber(newNumber))
        } else {
          val leftCount = left.getOpenCount()
          if (point > leftCount) {
            this.copy(left = left.addItemAtPoint(point, newNumber, newOperation))
          } else {
            this.copy(right = right.addItemAtPoint(point - leftCount, newNumber, newOperation))
          }
        }
      }

      override def toString(): String = {
        s"( ${left.toString} ${operationToString(operation)} ${right.toString} )"
      }
    }

    def operationToString(operation: OperationType.OperationType): String = {
      operation match {
        case OperationType.Plus     => "+"
        case OperationType.Minus    => "-"
        case OperationType.Multiply => "*"
        case OperationType.Divide   => "/"
      }
    }

    case class TreeNumber(value: Int) extends TreePoint {
      override def calc(): Double = value.toDouble

      override def getOpenCount(): Int = {
        1
      }

      override def addItemAtPoint(point: Int, newNumber: Int, newOperation: OperationType.OperationType): TreePoint = {
        TreeBranch(newOperation, this, TreeNumber(newNumber))
      }

      override def toString(): String = {
        "" + value
      }
    }

    def tempRecursionA(
      currentTree: TreePoint,
      nextC: Int,
      nextB: OperationType.OperationType,
      numbersWithoutCurrent: Seq[(Int, Int)],
      operationsWithoutCurrent: OperationHolder,
      target: Double,
      runningSeq: Seq[Int],
      testResult: Option[TreePoint],
    ): Option[TreePoint] = {
      (testResult, runningSeq.length) match {
        case (Some(_), _) => testResult
        case (None, 0)    => None
        case (None, _) =>
          val next = runningSeq.head
          val nextRunningSeq = runningSeq.tail
          tempRecursionA(
            currentTree,
            nextC,
            nextB,
            numbersWithoutCurrent,
            operationsWithoutCurrent,
            target,
            nextRunningSeq,
            recursionFunction(
              numbersWithoutCurrent,
              operationsWithoutCurrent,
              currentTree.addItemAtPoint(next, nextC, nextB),
              target,
            ),
          )
      }
    }

    case class OperationHolder(plusNum: Int, minusNum: Int, multNum: Int, divideNum: Int) {

      def stillHaveSome: Boolean = {
        (plusNum + minusNum + multNum + divideNum) > 0
      }

      def getOperation: (Option[OperationType.OperationType], OperationHolder) = {
        if (plusNum > 0) {
          (Some(OperationType.Plus), this.copy(plusNum = plusNum - 1))
        } else if (minusNum > 0) {
          (Some(OperationType.Minus), this.copy(minusNum = minusNum - 1))
        } else if (multNum > 0) {
          (Some(OperationType.Multiply), this.copy(multNum = multNum - 1))
        } else if (divideNum > 0) {
          (Some(OperationType.Divide), this.copy(divideNum = divideNum - 1))
        } else {
          (None, this)
        }
      }

      def remove(operationToRemove: OperationType.OperationType) = {
        operationToRemove match {
          case OperationType.Plus     => this.copy(plusNum = plusNum - 1)
          case OperationType.Minus    => this.copy(minusNum = minusNum - 1)
          case OperationType.Multiply => this.copy(multNum = multNum - 1)
          case OperationType.Divide   => this.copy(divideNum = divideNum - 1)
        }
      }
    }

    def tempRecursionB(
      currentTree: TreePoint,
      nextC: Int,
      numbersWithoutCurrent: Seq[(Int, Int)],
      remainingOperations: OperationHolder,
      target: Double,
      runningSeq: OperationHolder,
      testResult: Option[TreePoint],
    ): Option[TreePoint] = {
      (testResult, runningSeq.getOperation) match {
        case (Some(_), (_, _)) => testResult
        case (None, (None, _)) => None
        case (None, (Some(nextB), runningSeqNext)) =>
          val remainingOperationsNext = remainingOperations.remove(nextB)

          tempRecursionB(
            currentTree,
            nextC,
            numbersWithoutCurrent,
            remainingOperations,
            target,
            runningSeqNext,
            tempRecursionA(
              currentTree,
              nextC,
              nextB,
              numbersWithoutCurrent,
              remainingOperationsNext,
              target,
              (0 to currentTree.getOpenCount()),
              None,
            ),
          )
      }
    }

    def tempRecursionC(
      currentTree: TreePoint,
      remainingNumbers: Seq[(Int, Int)],
      remainingOperations: OperationHolder,
      target: Double,
      runningSeq: Seq[(Int, Int)],
      testResult: Option[TreePoint],
    ): Option[TreePoint] = {
      (testResult, runningSeq.length) match {
        case (Some(_), _) => testResult
        case (None, 0)    => None
        case (None, _) =>
          val nextC = runningSeq.head._1
          val nextCPos = runningSeq.head._2
          val runningSeqNext = runningSeq.tail
          val remainingNumbersNext = remainingNumbers.filterNot(_._2 == nextCPos)
          tempRecursionC(
            currentTree,
            remainingNumbers,
            remainingOperations,
            target,
            runningSeqNext,
            tempRecursionB(
              currentTree,
              nextC,
              remainingNumbersNext,
              remainingOperations,
              target,
              remainingOperations,
              None,
            ),
          )
      }
    }

    def recursionFunction(
      remainingNumbers: Seq[(Int, Int)],
      remainingOperations: OperationHolder,
      currentTree: TreePoint,
      target: Double,
    ): Option[TreePoint] = {
      ((currentTree.calc() == target), (remainingNumbers.nonEmpty && remainingOperations.stillHaveSome)) match {
        case (true, _)      => Some(currentTree)
        case (false, false) => None
        case (false, true) => {
          tempRecursionC(currentTree, remainingNumbers, remainingOperations, target, remainingNumbers, None)
        }
      }
    }

    val iWithIndex = i.zipWithIndex

    val operations = OperationHolder(o.count(_ == "+"), o.count(_ == "-"), o.count(_ == "*"), o.count(_ == "/"))

    iWithIndex.iterator
      .map {
        case (num, numPos) =>
          recursionFunction(
            iWithIndex.filterNot(_._2 == numPos),
            operations,
            TreeNumber(num),
            t.toDouble,
          )
      }
      .dropWhile(_.isEmpty)
      .nextOption()
      .flatten
      .map(_.toString())
  }

  def challengeFunctionAlecFastINVALID22(i: Seq[Int], o: Seq[String], t: Int): Option[String] = {

    val target: Double = t.toDouble

    abstract class TreePoint {
      def calc: Double
      def getOpenCount: Int
      def addItemAtPoint(point: Int, newNumber: Double, newOperation: String): TreePoint
      def toString: String
    }

    case class TreeBranch(operation: String, left: TreePoint, right: TreePoint) extends TreePoint {
      override def calc: Double = {
        operation match {
          case "+" => left.calc + right.calc
          case "-" => left.calc - right.calc
          case "*" => left.calc * right.calc
          case "/" => left.calc / right.calc
        }
      }

      override def getOpenCount: Int = left.getOpenCount + right.getOpenCount

      override def addItemAtPoint(point: Int, newNumber: Double, newOperation: String): TreePoint = {
        if (point == 0) {
          TreeBranch(newOperation, this, TreeNumber(newNumber))
        } else {
          val leftCount = left.getOpenCount
          if (point > leftCount) {
            this.copy(left = left.addItemAtPoint(point, newNumber, newOperation))
          } else {
            this.copy(right = right.addItemAtPoint(point - leftCount, newNumber, newOperation))
          }
        }
      }

      override def toString: String = s"( ${left.toString} $operation ${right.toString} )"
    }

    case class TreeNumber(value: Double) extends TreePoint {
      override def calc: Double = value

      override def getOpenCount: Int = 1

      override def addItemAtPoint(point: Int, newNumber: Double, newOperation: String): TreePoint = {
        TreeBranch(newOperation, this, TreeNumber(newNumber))
      }

      override def toString: String = "" + value
    }

    def tempRecursionA(
      currentTree: TreePoint,
      nextC: Int,
      nextB: String,
      numbersWithoutCurrent: Seq[(Int, Int)],
      operationsWithoutCurrent: Seq[(String, Int)],
      runningSeq: Seq[Int],
      testResult: Option[TreePoint],
    ): Option[TreePoint] = {
      (testResult, runningSeq.length) match {
        case (Some(_), _) => testResult
        case (None, 0)    => None
        case (None, _) =>
          tempRecursionA(
            currentTree,
            nextC,
            nextB,
            numbersWithoutCurrent,
            operationsWithoutCurrent,
            runningSeq.drop(1),
            recursionFunction(
              numbersWithoutCurrent,
              operationsWithoutCurrent,
              currentTree.addItemAtPoint(runningSeq.head, nextC, nextB),
            ),
          )
      }
    }

    def tempRecursionB(
      currentTree: TreePoint,
      nextC: Int,
      numbersWithoutCurrent: Seq[(Int, Int)],
      remainingOperations: Seq[(String, Int)],
      runningSeq: Seq[(String, Int)],
      testResult: Option[TreePoint],
    ): Option[TreePoint] = {
      (testResult, runningSeq.length) match {
        case (Some(_), _) => testResult
        case (None, 0)    => None
        case (None, _) =>
          val (nextB, nextBPos) = runningSeq.head
          tempRecursionB(
            currentTree,
            nextC,
            numbersWithoutCurrent,
            remainingOperations,
            runningSeq.drop(1),
            tempRecursionA(
              currentTree,
              nextC,
              nextB,
              numbersWithoutCurrent,
              remainingOperations.filterNot(_._2 == nextBPos),
              (0 to currentTree.getOpenCount),
              None,
            ),
          )
      }
    }

    def tempRecursionC(
      currentTree: TreePoint,
      remainingNumbers: Seq[(Int, Int)],
      remainingOperations: Seq[(String, Int)],
      runningSeq: Seq[(Int, Int)],
      testResult: Option[TreePoint],
    ): Option[TreePoint] = {
      (testResult, runningSeq.length) match {
        case (Some(_), _) => testResult
        case (None, 0)    => None
        case (None, _) =>
          val (nextC, nextCPos) = runningSeq.head
          tempRecursionC(
            currentTree,
            remainingNumbers,
            remainingOperations,
            runningSeq.drop(1),
            tempRecursionB(
              currentTree,
              nextC,
              remainingNumbers.filterNot(_._2 == nextCPos),
              remainingOperations,
              remainingOperations,
              None,
            ),
          )
      }
    }

    def recursionFunction(
      remainingNumbers: Seq[(Int, Int)],
      remainingOperations: Seq[(String, Int)],
      currentTree: TreePoint,
    ): Option[TreePoint] = {
      (currentTree.calc == target, remainingNumbers.nonEmpty && remainingOperations.nonEmpty) match {
        case (true, _)      => Some(currentTree)
        case (false, false) => None
        case (false, true)  => tempRecursionC(currentTree, remainingNumbers, remainingOperations, remainingNumbers, None)
      }
    }

    val iWithIndex = i.zipWithIndex

    iWithIndex.iterator
      .map {
        case (num, numPos) =>
          recursionFunction(
            iWithIndex.filterNot(_._2 == numPos),
            o.zipWithIndex,
            TreeNumber(num.toDouble),
          )
      }
      .dropWhile(_.isEmpty)
      .nextOption()
      .flatten
      .map(_.toString())
  }

  def challengeFunctionLukeShort(i: Seq[Int], o: Seq[String], t: Int): Option[String] = {
    def k(a: Seq[(Double, String)], b: Seq[String]): Option[String] = {
      for {
        (x, f) +: (z, g) +: _ <- a combinations 2
        y <- b
        h = y match {
          case "+" => z + x
          case "-" => z - x
          case "*" => z * x
          case _   => z / x
        }
        j = s"( $g $y $f )"
      } yield
        if (h == t)
          Some(j)
        else
          k((h, j) +: a diff Seq((x, f), (z, g)), b diff y +: "")
    }.flatten.nextOption

    k(i map (s => (s.toDouble, s + "")), o)
  } // 285

  def challengeFunctionLukeFast(i: Seq[Int], o: Seq[String], t: Int): Option[String] = {
    final case class AnswerFound(private val message: String, private val cause: Throwable = None.orNull) extends Exception(message, cause)

    case class Tree(left: Tree, value: String, right: Tree)

    def calculateTree(rootNode: Tree): Double = rootNode.value match {
      case "+" => calculateTree(rootNode.left) + calculateTree(rootNode.right)
      case "-" => calculateTree(rootNode.left) - calculateTree(rootNode.right)
      case "*" => calculateTree(rootNode.left) * calculateTree(rootNode.right)
      case "/" => calculateTree(rootNode.left) / calculateTree(rootNode.right)
      case _ => rootNode.value.toDouble
    }

    def treeToString(rootNode: Tree): String = rootNode.value match {
      case "+" | "-" | "*" | "/" =>
        s"( ${treeToString(rootNode.left)} ${rootNode.value} ${treeToString(rootNode.right)} )"
      case _ => rootNode.value
    }

    def solve(ints: Seq[String], operands: Seq[String], tree: Tree): Unit = {
      if (calculateTree(tree) == t)
        throw AnswerFound(treeToString(tree))
      else
        for {
          operand <- operands
          newOperands = operands diff Seq(operand)
          int <- ints
          newInts = ints diff Seq(int)
          newNode = Tree(null, int, null)
        } {
          solve(
            newInts,
            newOperands,
            Tree(newNode, operand, tree)
          )
          solve(
            newInts,
            newOperands,
            Tree(tree, operand, newNode)
          )
        }
    }

    def k(a: Seq[(Double, String)], b: Seq[String]): Option[String] = {
      for {
        (x, f) +: (z, g) +: _ <- a combinations 2
        y <- b
        h = y match {
          case "+" => z + x
          case "-" => z - x
          case "*" => z * x
          case _ => z / x
        }
        j = s"( $g $y $f )"
      } yield
        if (h == t)
          throw AnswerFound(j)
        else
          k((h, j) +: a diff Seq((x, f), (z, g)), b diff y +: "")
    }.flatten.nextOption

    try {
      // Try to solve with tree method
      val strI = i.map(_.toDouble.toString)

      for (int <- strI)
        solve(strI diff Seq(int), o, Tree(null, int, null))

      // If tree method fails, use short method as backup
      k(i map (s => (s.toDouble, s + "")), o)
    } catch {
      case ans: AnswerFound => Some(ans.getMessage)
    }
  }
}
