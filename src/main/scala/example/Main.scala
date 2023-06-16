package example

import org.openjdk.jmh.annotations.{Benchmark, BenchmarkMode, Mode, OutputTimeUnit}
import org.openjdk.jmh.infra.Blackhole

import java.util.concurrent.TimeUnit

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
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
  def testBenchmarkFunction(bh: Blackhole): Unit = {
    val i = Seq(1, 2, 3, 4, 5, 6, 7, 8, 9)
    val ops = Seq("*", "/", "+", "-")
    val t = 10
    bh.consume(challengeFunction(i, ops, t))
  }

  def challengeFunction(i: Seq[Int], o: Seq[String], t: Int): Option[String] = {
    None
  }
}
