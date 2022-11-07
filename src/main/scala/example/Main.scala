package example

object Main {

  def main(args: Array[String]): Unit = {
    timeFunction(challengeFunctionAlec("10,5"), "Alec", 5, 20, 1000000)
    timeFunction(challengeFunctionAamir("10,5"), "Aamir", 5, 20, 1000000)
    timeFunction(challengeFunctionBen("10,5"), "Ben", 5, 20, 1000000)
    timeFunction(challengeFunctionLuke("10,5"), "Luke", 5, 20, 1000000)
    timeFunction(challengeFunctionTom("10,5"), "Tom", 5, 20, 1000000)
    timeFunction(challengeFunctionDilyan("10,5"), "Dilyan", 5, 20, 1000000)
    timeFunction(challengeFunctionJacob("10,5"), "Jacob", 5, 20, 1000000)
  }
//jmh jvm micro benchmarking horse
  /*
Average time taken: 212 ms
Average time taken: 291 ms
Average time taken: 305 ms
Average time taken: 449 ms
Average time taken: 241 ms
Average time taken: 239 ms
   */

  def timeFunction[R](block: => R, name: String, preloadCount: Int, rerunCount: Int, iterations: Int) = {
    for (_ <- 1 to preloadCount) {
      block
    }
    val tempSeq = Seq.fill(rerunCount)(0)
    val output = tempSeq.map { _ =>
      time {
        for (_ <- 1 to iterations) {
          block
        }
      }
    }
    val calc = output.sum / output.length
    println(s"Average time taken: $calc ms")
  }

  /*
  CHALLENGE 2
  The input string will be a positive integer then a comma then another positive integer

  What do I do with these two positive integers?

  Convert the first number to binary, with the length of the output string dictated by the second integer

  "10,5" => "01010"
  "7,10" => "0000000111"
   */

  def challengeFunction(i: String) = {
    val q = i.split(",").map(_.toInt)
    "0" * q(1) + q(0).toBinaryString takeRight q(1)
  }
  //108 characters

  def challengeFunctionLong(i: String) = {
    val q = i.split(",").map(_.toInt)
    val l = q.apply(1)
    var r = q.apply(0)
    var s = Seq.empty[Int]
    while (r > 0) {
      s = s :+ r % 2
      r = r >> 1
    }
    s.take(l).padTo(l, "0").reverse.mkString
  }
  //182 characters

  def challengeFunctionTest(i: String) = {
    val q = i.split(",").map(_.toInt)
    var s = ""
    while (q(0) > 0) {
      s = q(0) % 2 + s
      q(0) /= 2
    }
    "0" * q(1) + s takeRight q(1)
  }

  def challengeFunction147(i: String) = {
    val q = i.split(",").map(_.toInt)
    Seq
      .fill(q.apply(1))(0)
      .zipWithIndex
      .map { a =>
        (q.apply(0) >> a._2) % 2
      }
      .reverse
      .mkString
  }
  //147 characters

  def challengeFunction132(i: String) = {
    val q = i.split(",").map(_.toInt)
    Seq
      .range(0, q.apply(1))
      .map { a =>
        (q.apply(0) >> a) % 2
      }
      .reverse
      .mkString
  }
  //132 characters

  def challengeFunction119(i: String) = {
    val q = i.split(",").map(_.toInt)
    Seq
      .range(0, q(1))
      .map { a =>
        (q(0) >> a) % 2
      }
      .reverse
      .mkString
  }
  //119 characters

  def challengeFunction118(i: String) = {
    val q = i.split(",")
    Seq
      .range(0, q(1).toInt)
      .map { a =>
        (q(0).toInt >> a) % 2
      }
      .reverse
      .mkString
  }
  //118 characters

  def challengeFunction113(i: String) = {
    val ^ = i.split(",")
    (0 to ^(1).toInt - 1)
      .map { a =>
        (^(0).toInt >> a) % 2
      }
      .reverse
      .mkString
  }
  //def challengeFunction(i:String)={val^ =i.split(",");(0 to^(1).toInt-1).map{a=>(^(0).toInt>>a)%2}.reverse.mkString}
  //113 characters

  def challengeFunctionAlec(i: String) = {
    val q = i.split(",")
    (q(1).toInt - 1 to 0 by -1).map { a =>
      (q(0).toInt >> a) % 2
    }.mkString
  }
  //def challengeFunction(i:String)={val q=i.split(",");(q(1).toInt-1 to 0by-1).map{a=>(q(0).toInt>>a)%2}.mkString}
  //111 characters

  def challengeFunctionAamir(i: String): String = {
    def t(n: Int): String = n match {
      case 0 | 1 => s"$n"
      case _     => s"${t(n / 2)}${n % 2}"
    }
    val a = i.split(",")
    val n = a(1).toInt
    (("0" * n) + t(a(0).toInt)).takeRight(n)
  }
  //183 characters

  def challengeFunctionBen(c: String) = {
    val Array(a, b) = c.split(",").map(_.toInt)
    def f(i: Int, s: String, v: Int): String = {
      if (i < 1) return s
      val p = Math.pow(2, i - 1).toInt
      if (v < p) f(i - 1, s + 0, v) else f(i - 1, s + 1, v - p)
    }
    ("0" * b + f(a, "", a)).takeRight(b)
  }
  //221 characters

  def challengeFunctionLuke(s: String) = {
    var Array(a, b) = s.split(",").map(_.toInt)
    var f = ""
    while (a > 0) {
      f = s"${a % 2}" + f
      a /= 2
    }
    ("0" * b) + f takeRight b
  }
  //132 characters

  def challengeFunctionLukeImproved(s: String) = {
    var Array(a, b) = s.split(",").map(_.toInt)
    var f = ""
    while (a > 0) {
      f = a % 2 + f
      a /= 2
    }
    ("0" * b) + f takeRight b
  }
  //126 characters

  def challengeFunctionTom(i: String) = {
    def t(k: Int): Int = if (k == 0 || k == 1) k else (k % 2) + 10 * t(k / 2)
    val a = i.split(",").map(_.toInt)
    (("0" * 7) + t(a(0))).takeRight(a(1))
  }
  //150 characters

  def challengeFunctionDilyan(i: String) = {
    val s = i.split(",")
    val b = s(0).toInt.toBinaryString
    val l = s(1).toInt
    b.++:("0" * (l - b.size)).takeRight(l)
  }
  //133 characters

  def challengeFunctionJacob(i: String): String = {
    val x = i.split(",")
    LazyList
      .iterate(x(0).toInt -> "") { k =>
        (k._1 / 2, k._1 % 2 + k._2)
      }(x(1).toInt)
      ._2
  }
  //131 characters (124 if all removed)

  def time[R](block: => R) = {
    val t0 = System.currentTimeMillis()
    block
    val t1 = System.currentTimeMillis()
    //println("Elapsed time: " + (t1 - t0) + "ms")
    t1 - t0
  }

}
