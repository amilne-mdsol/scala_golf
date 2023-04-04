package example

import org.openjdk.jmh.annotations.{Benchmark, BenchmarkMode, Mode, OutputTimeUnit}
import org.openjdk.jmh.infra.Blackhole

import java.util.concurrent.TimeUnit

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
object Main {

  @Benchmark
  def testBenchmarkFunction(bh: Blackhole): Unit = {
    val w = ""
    bh.consume(challengeFunction(w))
  }

  def challengeFunction(i: String): Boolean = {
    true
  }
}
