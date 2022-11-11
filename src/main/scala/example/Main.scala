package example

object Main {

  def main(args: Array[String]): Unit =
    println("Hello com.example.Empty Project!")

  /*
  Challenge 3: Find the mountains
  The function will be given a Seq of Integers
  You need to output a Seq of Truples of all the "peaks" in the data

  e.g.
  Input: 1,5,1,6,7,8,7,6,8
  Output: (1,5,1),(7,8,7)

  Input: 1,2,3,4,5,6,7,8,9,1
  Output: (8,9,1)

  Input: 0,0,0,0,5,0,0,0,0
  Output: (0,5,0)
   */

  def challengeFunctionSliding(i: Seq[Int]): Seq[(Int, Int, Int)] = {
    i.sliding(3, 1).toSeq.filter(a => a(1) > a(0) && a(1) > a(2)).map(a => (a(0), a(1), a(2)))
  }

  def challengeFunctionZipped(i: Seq[Int]): Seq[(Int, Int, Int)] = {
    (i.dropRight(2), i.tail.dropRight(1), i.drop(2)).zipped.toSeq.filter(a => a._2 > a._1 && a._2 > a._3)
  }

  def challengeFunctionA(i: Seq[Int]): Seq[(Int, Int, Int)] = {
    i.zipWithIndex.drop(1).dropRight(1).flatMap { c =>
      val l = i(c._2 - 1)
      val m = c._1
      val r = i(c._2 + 1)
      if (m > l && m > r) {
        Seq((l, m, r))
      } else {
        Seq()
      }
    }
  }

  def challengeFunctionB(i: Seq[Int]): Seq[(Int, Int, Int)] = {
    i.zipWithIndex.drop(1).dropRight(1).flatMap { c =>
      val (l, m, r) = (i(c._2 - 1), c._1, i(c._2 + 1))
      if (m > l && m > r) {
        Seq((l, m, r))
      } else {
        Seq()
      }
    }
  }

  def challengeFunction222(i: Seq[Int]): Seq[(Int, Int, Int)] = {
    i.zipWithIndex.drop(1).dropRight(1).flatMap {
      case (x, c) =>
        val l = i.take(c + 1).reverse.takeWhile(_ <= x)
        val r = i.takeRight(i.length - c).takeWhile(_ <= x)
        if (l.distinct.length > 1 && r.distinct.length > 1) {
          Seq((i(c - 1), i(c), i(c + 1)))
        } else {
          Seq()
        }
    }
  } //222

  def challengeFunction220(i: Seq[Int]): Seq[(Int, Int, Int)] = {
    i.zipWithIndex.drop(1).dropRight(1).flatMap {
      case (x, c) =>
        val (l2, r2) = i.splitAt(c)
        val l = (l2 :+ x).reverse.takeWhile(_ <= x)
        val r = r2.takeWhile(_ <= x)
        if (l.distinct.length > 1 && r.distinct.length > 1) {
          Seq((i(c - 1), i(c), i(c + 1)))
        } else {
          Seq()
        }
    }
  } //220

  def challengeFunction198(i: Seq[Int]): Seq[(Int, Int, Int)] = {
    i.zipWithIndex.drop(1).dropRight(1).flatMap {
      case (x, c) =>
        val b = i.splitAt(c)
        val z = Seq((b._1 :+ x).reverse, b._2).map(_.takeWhile(_ <= x).distinct.length > 1)
        if (z(0) && z(1)) {
          Seq((i(c - 1), i(c), i(c + 1)))
        } else {
          Seq()
        }
    }
  } //198

  def challengeFunction174(i: Seq[Int]): Seq[(Int, Int, Int)] = {
    i.zipWithIndex.flatMap {
      case (x, c) =>
        val b = i.splitAt(c)
        val z = Seq((b._1 :+ x).reverse, b._2).map(_.takeWhile(_ <= x).distinct.length > 1)
        if (z(0) && z(1)) {
          Seq((i(c - 1), x, i(c + 1)))
        } else {
          Seq()
        }
    }
  } //174

  def challengeFunction(i: Seq[Int]): Seq[(Int, Int, Int)] = {
    i.zipWithIndex.flatMap {
      case (x, c) =>
        val b = i.splitAt(c)
        if (Seq((b._1 :+ x).reverse, b._2).forall(_.takeWhile(_ <= x).distinct.length > 1))
          Seq((i(c - 1), x, i(c + 1)))
        else
          Seq()
    }
  } //158

  def challengeFunctionFoldLeft(i: Seq[Int]): Seq[(Int, Int, Int)] = {
    i.zipWithIndex.foldLeft(Seq[(Int, Int, Int)]()) {
      case (y, (x, c)) =>
        val b = i.splitAt(c)
        val z = Seq((b._1 :+ x).reverse, b._2).forall(_.takeWhile(_ <= x).distinct.length > 1)
        if (z) {
          y :+ (i(c - 1), x, i(c + 1))
        } else {
          y
        }
    }
  } //195

  def challengeFunctionBen(x: Seq[Int]) = {
    x.drop(1)
      .zip(x)
      .map(t => t._1 - t._2)
      .zipWithIndex
      .foldLeft((Seq[(Int, Int, Int)](), Seq[Int]())) {
        case ((a, p), (n, i)) =>
          var q = p
          var b = a
          if (n < 0) {
            b = a ++ p.map(y => (x(y), x(y + 1), x(y + 2)))
            q = Seq()
          } else if (n == 0) {
            if (p.size > 0) {
              q = p :+ i
            }
          } else {
            q = Seq(i)
          }
          (b, q)
      }
      ._1
  } //231

  def challengeFunctionLuke(i: Seq[Int]): Seq[(Int, Int, Int)] = {
    var l = i(0)
    var p = Seq(l)
    var r = Seq[(Int, Int, Int)]()
    for (f <- i.tail) {
      if (p.size > 1) {
        p :+= f
        if (f < l) {
          if (p.size > 2) r = r ++ (p, p.tail, p.tail.tail).zipped.toList
          p = Seq()
        }
      }
      if (f > l) p = Seq(l, f)
      l = f
    }
    r
  } //170

  def challengeFunctionJacob(i: Seq[Int]) = {
    val e = Seq[Seq[Int]]()
    def x[T](b: Boolean, t: T, f: => T) = if (b) t else f
    i.sliding(2)
      .foldLeft((e, Seq[(Int, Int, Int)]())) {
        case ((t, s), n) =>
          x(
            n(0) > n(1),
            x(t.isEmpty, (e, s), (e, s ++ ((t :+ n).map(_(0)) :+ n(1)).sliding(3).map(f => (f(0), f(1), f(2))))),
            x(n(0) < n(1), (Seq(n), s), x(t.isEmpty, (e, s), (t :+ n, s))),
          )
      }
      ._2
  } //279
}
