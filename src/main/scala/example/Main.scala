package example

object Main {

  /*
  Challenge 4:

  Convert the integer input to a string output.

  Sounds easy?

  WRONG

  You have to convert the integer into a written version of the number

  1 => One
  2 => Two
  95716 => Ninety five thousand, seven hundred and sixteen
   */

  def challengeFunctionAlec(i: Int) = {
    val (p, s, n, v, f, e) = ("teen", "six", "seven", "nine", "four", "eigh")
    val F = Seq(
      "",
      "one",
      "two",
      "three",
      f,
      "five",
      s,
      n,
      e + "t",
      v,
      "ten",
      "eleven",
      "twelve",
      "thir" + p,
      f + p,
      "fif" + p,
      s + p,
      n + p,
      e + p,
      v + p,
      "twen",
      "thir",
      "for",
      "fif",
      s,
      n,
      e,
      v,
    )

    val z = Seq((1000000000, " billion"), (1000000, " million"), (1000, " thousand"), (1, ""))
      .flatMap { x =>
        val y = i / x._1 % 1000
        val r = y % 100
        if (y > 0)
          Seq({
            { if (y >= 100) F(y / 100) + " hundred" + { if (r > 0) " and " else "" } else "" } + {
              if (r < 20) F(r) else F(r / 10 + 18) + { if (r % 10 > 0) "ty " + F(r % 10) else "ty" }
            }
          } + x._2)
        else Seq()
      }
      .mkString(", ")
    z.take(1).toUpperCase + z.tail
  } //513

  def challengeFunctionFrancesco(i: Int) = {
    import scala.math._
    val q = ", "
    val n = (1000000 -> "million") -> q
    val o = (1000 -> "thousand") -> q
    val h = "hundred"
    val p = " and "

    def m(i: Int) =
      List(
        100 -> h -> p,
        o,
        o,
        o,
        n,
        n,
        100000000 -> h -> p,
        1000000000 -> "billion" -> q,
      )(i - 2)
    val d = List(
      "twenty",
      "thirty",
      "forty",
      "fifty",
      "sixty",
      "seventy",
      "eighty",
      "ninety",
    )
    val c = List(
      "",
      "one",
      "two",
      "three",
      "four",
      "five",
      "six",
      "seven",
      "eight",
      "nine",
      "ten",
      "eleven",
      "twelve",
      "thirteen",
      "fourteen",
      "fifteen",
      "sixteen",
      "seventeen",
      "eighteen",
      "nineteen",
    )
    def b(j: Int, l: Int) =
      s"${f(j / m(l)._1._1)} ${m(l)._1._2}${if (j % m(l)._1._1 == 0) "" else s"${m(l)._2}${f(j % m(l)._1._1)}"}"
    def f(i: Int): String =
      if (i >= 100) b(i, log10(i).toInt)
      else if (i >= 20) s"${d((i / 10) - 2)}${if (i % 10 == 0) "" else s" ${f(i % 10)}"}"
      else c(i)
    f(i).capitalize
  } //691

  def challengeFunctionAamir(i: Int): String = {
    val t = 1000
    val h = 100
    val b = t * t * t
    val m = b / t
    val x = Seq(
      "",
      "one",
      "two",
      "three",
      "four",
      "five",
      "six",
      "seven",
      "eight",
      "nine",
      "ten",
      "eleven",
      "twelve",
      "thirteen",
      "fourteen",
      "fifteen",
    )

    val c = Seq("", "", "twenty ", "thirty ", "forty ", "fifty ")
    def g(n: Int, b: Int, t: String) = {
      val m = n % b
      val d = f(n / b)
      if (m > h) s"$d $t, ${f(m)}"
      else if (m < h && m > 0) s"$d $t and ${f(m)}"
      else s"$d $t ${f(m)}"
    }

    def f(n: Int): String = {
      if (n >= b) g(n, b, "billion")
      else if (n >= m) g(n, m, "million")
      else if (n >= t) g(n, t, "thousand")
      else if (n >= h) {
        val d = f(n / h)
        (if (n % h > 0) d + " hundred and " else d + " hundred ") + f(n % h)
      } else if (n >= 20) {
        val j = n / 10
        (if (j < 6) c(j)
         else f(j).stripSuffix("t") + "ty ") + f(n % 10)
      } else if (n >= 0 && n < 16) x(n)
      else f(n - 10).stripSuffix("t") + "teen"
    }
    val s = f(i)
    s(0).toUpper + s.tail
      .replaceAll(" +", " ")
      .trim
  } //694

  def challengeFunctionLuke(i: Int): String = {
    val s = "teen"
    val b = Seq(
      "",
      " one",
      " two",
      " three",
      " four",
      " five",
      " six",
      " seven",
      " eight",
      " nine",
      " ten",
      " eleven",
      " twelve",
      " thir" + s,
      " four" + s,
      " fif" + s,
      " six" + s,
      " seven" + s,
      " eigh" + s,
      " nine" + s,
    )

    ("" + i).reverse
      .grouped(3)
      .map(_.reverse.mkString.toInt)
      .zip(Seq("", " thousand", " million", " billion"))
      .toSeq
      .reverse
      .flatMap {
        case (x, y) =>
          val a = x % 100
          if (x > 0)
            Seq(
              ",",
              if (x > a) b(x / 100) + " hundred",
              if (x > a & a > 0) " and",
              if (a > 19)
                Seq(
                  " twen",
                  " thir",
                  " for",
                  " fif",
                  b(6),
                  b(7),
                  " eigh",
                  b(9),
                ).map(_ + "ty")(a / 10 - 2) + b(a % 10)
              else
                b(a),
              y,
            ).filter(_ != ())
          else
            ""
      }
      .mkString
      .drop(2)
      .capitalize
  } //522
}
