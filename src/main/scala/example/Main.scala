package example

import org.openjdk.jmh.annotations.{Benchmark, BenchmarkMode, Mode, OutputTimeUnit}
import org.openjdk.jmh.infra.Blackhole

import java.util.concurrent.TimeUnit

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
class Main {

  /*
    Challenge 9
    Regular palindromes

    Given a string, find all the palindromes within it.

    Palindromes in this challenge can have regular gaps between every letter. They can also be without gaps.

    Minimum length of a palindrome in this challenge is 3 characters.

    For example
    "Hannah" contains the palindrome "hannah". It also contains sub-palindromes, but we only want the largest. No "anna".

    But, we are also interested in when there are gaps.

    For example
    (in this instance, - means an unusable character to aid in reading this example.)
    "H--a--n--n--a--h" contains the palindrome "hannah" with a jump of 2 characters.

    Ignore capitalisation in the input word, all returned palindromes should be lower-case

    Sub palindromes are in some instances allowed.

    For example
    "abcbadefedabcba"
    the whole word is a palindrome, but the start and end are also individual palindromes that are valid.
    "abcbadefedabcba"
    "abcba" with 2, as there is one at the start and one at the end

    The return structure isn't just returning the palindrome strings. You need to build the response as a Seq with this format for each item

    (found_palindrome, start_position, jump)

    For example
    Input string -> "h11a11n22n22a33h"
    Response -> Seq(("hannah", 0, 2), ("11a11", 1, 0), ("n22n", 6, 0), ("22n22", 7, 0))

    They need to be ordered depending on where they were found in the input. If two palindromes started at the same character, put one that ends sooner first.

    Notes:
      The function declaration is not to be altered. These characters will not be added to the character count.
      Don't use Mutable objects.
      All the code for your solution needs to be within the function. No external functions, imports or values.

    More tests may be added during the challenge, but this will be highlighted in the scala enthusiasts channel on Slack
   */

  /*
  Here is an example of how to structure a benchmarking test.

  Just copy/paste this block with a different function name to add more benchmark tests

  To run it use:
  Jmh/run

  In the sbt shell
   */
  @Benchmark
  def testBenchmarkFunction(bh: Blackhole): Unit = {
    val w =
      "yGyEjnhxJsfjqUFbmaATbKalWRexURzKIIgFzUXJNRVAUYcExiNUoRViqqDjAzBauPKEcwFoGUBgkWRnZnpKFCdSHLZtMnsCmHlldfAZzCRwYQGozuBiDZRmNsEGnLMjGddTSFHDntwFAcWtXgMEoeHsPWDAjHgoZwwVRESkgOyroUQXqjAVXcUxKSCokETJMHMMTYjFDwKenJqlzOZjRsFlZMVBcupAqANwyEOMEvRfeLbqpfLWizoClnBTlsmmTRaPscTsTmOmzHiPDGvcWBuhWORwLQjGqdKJHUHUmxdOSFEzPGfuSbkuPnuinuIxGeyqyJjAgPEszKKhQNqrqUsVtehRjRWggcOQXwolzkazkpaIwOtHAwylrFPsORtyEUwlxPUAWDYAxUOlujvbvYinxCWfvRRbwDYAnMCLVlVufoLnqYKKwPkxwVEZIlyrrSsjYqdauEdRthGkVimskzPbDhlhorWpJBcaEkOTWzQLgAcgNhUZsxcMecBVcJYEeyTKVxxgKUPemMqfhIzhVFFSanVSwpdPeesDSZhgoGspQmKGnncBoCVpocjVYstZHweHyaDvIxPArxSxYAWQhlwdajxKtXNiSsgEtwOrUrUtvTEqWtwGZLgvHezeAfaTCVsOylfgOacJjooxNnVhaNCnDHtzaXkQysXqAclPljuAgUgbZDmnErCySNhIEJoeLJvzKRpRdSuOhDjMUdvHqdVQYcCSFaQZMsnmUPkArWfqphXXeICaQKORrtlrUAlmqdQoydXLOCtahQcakfXESABxuOhyMGEduVNHDfXNSEwTgrvBSUrFlHZGIQSsznxuKlMSjaBLnQWrXniKzytpSSkrWHzAmhkHkZRpiDFgWruzYOEmfITWQgfuKnmNniZFNFpLYWgpdlumdgPtuDGRhtkwaPAxEMjAqiQhyqzmTpUVJyiDfbjmifNBErbewBoUDrgCvjQEGKLGnHPDlodNKudrrBMCTQbHfRSSyIwm"
    bh.consume(challengeFunction(w))
  }

  def challengeFunction(w: String): Seq[(String, Int, Int)] = {
    challengeFunctionLukeSmall(w)
    challengeFunctionLukeFast(w)
    challengeFunctionBenFast(w)
    challengeFunctionTomFast(w)
    challengeFunctionAlecFast(w)
    challengeFunctionAlecSmall(w)
  }

  def challengeFunctionLukeSmall(w: String): Seq[(String, Int, Int)] = {
    val f =
      for {
        e <- w.indices
        t <- 1 to w.size
        o = (w.toLowerCase drop e sliding (1, t)).mkString
        a <- 3 to o.size map o.take
        if a == a.reverse
      } yield (a, a.indices map (e + _ * t), e, t - 1)

    (
      for {
        (a, b, c, d) <- f
        if !f.exists {
          case (_, x, y, z) =>
            (c, d) != (y, z) &
              c >= y &
              b.last <= x.last &
              (
                (
                  for {
                    j <- 0 to 1
                    p = x.size
                    i <- j to p
                    if j < 1 || p % i - 1 < 1
                  } yield if (j > 0) (x grouped i map (_(0))).toSeq else x slice (i, p - i)
                ) contains b
              )
        }
      } yield (a, c, d)
    ) sortBy {
      case (a, b, c) =>
        (b, (c + 1) * (a.size - 1), a.size)
    }
  } //395

  def challengeFunctionLukeFast(w: String): Seq[(String, Int, Int)] = {
    def isSubPalindrome(b: Seq[Int], x: Seq[Int]): Boolean = {
      val xLen = x.length

      if (b.length < xLen)
        if (x == b)
          return true
        else
          for {
            i <- 1 to xLen / 2
            if x.slice(i, xLen - i) == b || (xLen % i - 1 == 0 && x.grouped(i).map(_.head).toSeq == b)
          } return true

      false
    }

    val wLower = w.toLowerCase

    val palindromes =
      for {
        start <- wLower.indices
        step <- 1 to wLower.length / 2
        word = wLower.drop(start).sliding(1, step).mkString
        subWord <- 3 to word.length map word.take
        if subWord == subWord.reverse
      } yield (subWord, for (i <- subWord.indices) yield start + i * step, start, step)

    import scala.concurrent.{Await, Future}
    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.concurrent.duration.Duration

    val palindromeFuture = palindromes.map {
      case (a, b, c, d) =>
        Future {
          if (!palindromes.exists {
                case (_, x, y, z) =>
                  (c, d) != (y, z) &&
                    c >= y &&
                    b.last <= x.last &&
                    isSubPalindrome(b, x)
              })
            Some(a, c, d - 1)
          else
            None
        }
    }

    val res = Await.result(Future.sequence(palindromeFuture), Duration.Inf)

    res.flatten.sortWith {
      case ((a, b, c), (x, y, z)) =>
        if (b == y) {
          val aLen = a.length - 1
          val xLen = x.length - 1
          val n = (c + 1) * aLen - (z + 1) * xLen

          if (n == 0)
            aLen < xLen
          else
            n < 0
        } else
          b < y
    }
  }

  def challengeFunctionBenFast(w: String): Seq[(String, Int, Int)] = {

    val s = w.toLowerCase

    // axxxxb, axxxbx, axxbxx,
    // axbxxx, xaxxxb, xaxxbx,
    // xxaxxb, xxaxbx, xxxaxb
    // So solution will be at least O(n^2) :(
    def generateOuterAAndB(n: Int): Vector[(Int, Int)] = {
      (0 to (n - 3)).toVector.flatMap(a => ((a + 2) to (n - 1)).reverse.toVector.map(b => (a, b)))
    }

    // AaxxxxxbB, AxaxxxbxB, AxxaxbxxB
    def generateAAndB(outerA: Int, outerB: Int): Vector[(Int, Int)] = {
      val n = outerB - outerA + 1
      (0 to (n - 1) / 2).toVector.map(a => (a, n - 1 - a)).map { case (a, b) => (a + outerA, b + outerA) }
    }

    // true if the set generated by X entirely contains the set generated by Y
    def XCompletelyContainsY(X: (Int, Int, Int), Y: (Int, Int, Int)): Boolean = (X, Y) match {
      case ((x, _, z), (x1, y1, z1)) =>
        val n = (y1 - x1) / z1 + 1
        (0 to n).toVector.forall(i => (x1 - x + (i * z1)) % z == 0)
    }

    class Store(
      cache: Set[(Int, Int)],
      candidates: Vector[(Int, Int, Int)],
      val solutions: Vector[(String, Int, Int)],
      divisorsCache: Map[Int, Vector[Int]],
    ) {
      def checkCache(t: (Int, Int)): Boolean = cache.contains(t)

      def addCandidates(a: Int, b: Int, gs: Vector[Int]): Store = {
        val updatedCandidates = candidates ++ gs.map(g => (a, b, g))
        // println(s"  adding candidates ${gs.map(g => (a, b, g))}")
        // println(s"    candidates: $updatedCandidates")
        new Store(cache, updatedCandidates, solutions, divisorsCache).addCache((a, b))
      }

      def invalidate(a: Int, b: Int): Store = {
        // Complex thing to do #1 - invalidate candidates given a != b
        // A candidate is invalid if it generates the set which contains a.
        // A candidate generates the set S = { s, s + g, s + 2g, ... , s + mg } for some m.
        // if a is in S, then a = s + xg where x is an integer >=0
        // or a - s = xg
        // or (a - s) % g == 0
        val updatedCandidates = candidates.filterNot { case (s, _, g) => (a - s) % g == 0 }
        // println(s"  invalidating ${candidates.filter { case (s, _, g) => (a - s) % g == 0 }}")
        // println(s"    candidates after invalidating: $updatedCandidates")
        new Store(cache, updatedCandidates, solutions, divisorsCache).addCache((a, b))
      }

      def toSolution(candidate: (Int, Int, Int)): (String, Int, Int) = candidate match {
        case (a, b, g) => (s.slice(a, b + 1).grouped(g).map(_.head).mkString, a, g - 1)
      }

      def rollUp(): Store = {
        // Complex thing to do #2 - add solutions for existing candidates

        // candidate C becomes a solution if no preceding solutions generate a set which contains the set generated by C
        //
        // when does a set generated by (a,b,g) contain the set generated by (a',b',g')?
        //
        // (a,b,g) generates C
        // (a',b',g') generates C'
        //
        // C = { a + 0g, a + 1g, a + 2g, ... , b }
        // C' = { a' + 0g', a' + 1g', a' + 2g', ... , b' }
        //
        // a' + 0g' = a + xg for some x
        // a' + 1g' = a + xg for some x
        // a' + 2g' = a + xg for some x
        // ...
        // b' = a + xg for some x
        //
        //
        // for i = 0..n: a' + ig' = a + xg
        // for i = 0..n: a' + ig' - a = xg
        // for i = 0..n: (a' + ig' - a) % g == 0
        //
        // can we factor out the i?
        // for now let's brute force, which means we need to generate n

        val finalCandidates = candidates.foldLeft(Vector[(Int, Int, Int)]())((accumlateSolutions, Y) =>
          if (accumlateSolutions.exists(X => XCompletelyContainsY(X, Y))) {
            // println(s"    rejecting candidate: $Y")
            accumlateSolutions
          } else {
            // println(s"    accepting candidate: $Y")
            accumlateSolutions :+ Y
          },
        )
        val newSolutions = finalCandidates.map(fc => toSolution(fc))
        val finalSolutions = solutions ++ newSolutions
        // println(s"    final solutions: $finalSolutions")
        new Store(cache, Vector(), finalSolutions, divisorsCache)
      }

      def getDivisorsForNMinus1(n: Int): (Vector[Int], Store) =
        divisorsCache.get(n) match {
          case Some(divisors) => (divisors, this)
          case None =>
            val divisors = Store.calculateDivisorsOfNMinusOne(n)
            (divisors, new Store(cache, candidates, solutions, divisorsCache.updated(n, divisors)))
        }

      private def addCache(t: (Int, Int)): Store = new Store(cache + t, candidates, solutions, divisorsCache)

    }
    object Store {
      val empty: Store = new Store(Set(), Vector(), Vector(), Map())

      def calculateDivisorsOfNMinusOne(n: Int): Vector[Int] = {
        val odd = n % 2 == 1
        val gMax = if (odd) (n - 1) / 2 else (n - 1) / 3
        (1 to gMax).filter(g => (n - 1) % g == 0).toVector
      }
    }

    val N = s.length

    def innerFold(store: Store, innerTuple: (Int, Int)): Store = {
      val (a, b) = innerTuple
      val n = b - a + 1
      // println(s"  inner ($a, $b). n = $n: : ${s(a)} ${if (s(a) == s(b)) "==" else "!="} ${s(b)}")
      val (gs, store2) = store.getDivisorsForNMinus1(n)

      if (s(a) == s(b)) {
        store2.addCandidates(a, b, gs)
      } else {
        store2.invalidate(a, b)
      }
    }

    def outerFold(store: Store, outerTuple: (Int, Int)): Store = {
      val (outerA, outerB) = outerTuple
      if (store.checkCache(outerTuple)) {
        // println("Cache hit")
        store
      } else {
        generateAAndB(outerA, outerB).foldLeft(store.rollUp())(innerFold)
      }
    }

    val result = generateOuterAAndB(N)
      .foldLeft(Store.empty)(outerFold)
      .rollUp()

    def sortBy(t: (String, Int, Int)) = t match {
      case (s, a, gMinus1) =>
        val b = a + ((s.length - 1) * (gMinus1 + 1))
        (a, b, s.length)
    }

    val sorted = result.solutions.sortBy(sortBy)

    sorted
  }

  def challengeFunctionTomFast(w: String): Seq[(String, Int, Int)] = {

    val input = w.toLowerCase()

    //    def checkString(s: String, pos: Int = 0, acc: List[(String, Int, Int)] = List.empty): List[(String, Int, Int)] = {
    //      if (s.length <= 3) acc
    //      else if (s == s.reverse)
    //        checkString(s.tail, pos = 1, acc = acc.appended((s, 0, 0)))
    //      else checkString(s.tail, pos = 1, acc = acc)
    //    }

    def checkStringWithGaps(
      s: String,
      jump: Int = 0,
      pos: Int = 0,
      acc: List[(String, Int, Int)] = List.empty,
    ): List[(String, Int, Int)] = {

      val s2 = stringWithGaps(s, jump)

      println(s"CHECK WITH GAP - s=$s - jump = $jump - withJUMP = $s2 - pos = $pos - acc = $acc")

      if (s2.length < 3) acc
      else
        checkStringWithGaps(
          s.tail,
          jump,
          pos = pos + 1,
          acc = acc.concat(checkString(s2).map(s => (s, pos, jump))),
        )
    }

    def stringWithGaps(s: String, jump: Int): String = {
      s.foldLeft(("", 0)) {
          case ((stringAcc, i), c) if i == 0 || i > jump =>
            (stringAcc.appended(c), 1)

          case ((stringAcc, i), c) =>
            (stringAcc, i + 1)

        }
        ._1
    }

    //TODO detect if a string is inside another pal with the
    def checkString(s: String): List[String] = {
      val subStrings = getAllSubStrings(s)
      val res = subStrings.filter(s => s.length >= 3 && s == s.reverse && s != "anna").toList
      res
    }

    def getAllSubStrings(s: String): Seq[String] = {
      val len = s.length
      for {
        length <- 3 until len + 1
      } yield s.take(length)
    }

    def removeInnerWords(words: Seq[(String, Int, Int)]): Seq[(String, Int, Int)] = {
      words.filterNot {
        case (word, _, _) =>
          words.exists {
            case (widerWord, _, _) =>
              widerWord != word && (widerWord.contains(word) || stringWithGaps(widerWord, 1).contains(word))
          }
      }
    }

    val results: Seq[(String, Int, Int)] = (for {
      gapSize <- 0 until w.length
      words <- checkStringWithGaps(input, gapSize)
    } yield words)

    //not correct - needs end to get proper ordering. Its start pos, then the one that ends first.
    removeInnerWords(results).sortWith {
      case ((a, starta, gapa), (b, startb, gapb)) =>
        if (starta == startb) {
          if (gapa == gapb) a.length > b.length
          else gapa < gapb
        } else starta < startb
    }
  }

  def challengeFunctionAlecFast(w: String): Seq[(String, Int, Int)] = { // 324
    val length = w.length
    val wLower = w.toLowerCase

    val temp: Seq[Seq[(Char, Int)]] = for {
      index <- (0 to length - 3)

      remainingLength = length - index
      possibleJumpLimit: Int = Math.floor((remainingLength - 1) / 2).toInt

      jumpNum <- (0 until possibleJumpLimit)

      cNums = (index until length by jumpNum + 1)
      wordLength = Math.ceil((length - index) * 1d / (jumpNum + 1)).toInt

      c = cNums.map {
        wLower(_)
      }

      thisSampleChar <- (1 to (wordLength) / 2)

      t <- Seq(0, 1)
      if (thisSampleChar > 1 - t) &&
        thisSampleChar + t + thisSampleChar <= wordLength &&
        c(thisSampleChar - 1) == c(thisSampleChar + t) &&
        c(0) == c(thisSampleChar + thisSampleChar + t - 1) &&
        (c.take(thisSampleChar).reverse == c.drop(thisSampleChar + t).take(thisSampleChar))
    } yield {
      (c.take(thisSampleChar * 2 + t)).zipWithIndex.map { case (m, k) => (m, index + jumpNum * k + k) }
    }
    temp
      .foldLeft(Map[Double, Set[Seq[(Char, Int)]]]()) {
        (accum: Map[Double, Set[Seq[(Char, Int)]]], itemWithIndex: Seq[(Char, Int)]) =>
          val itemMiddle = (itemWithIndex(0)._2 + itemWithIndex.last._2) * 0.5
          accum.get(itemMiddle) match {
            case Some(theseItems) => {
              if (theseItems.exists { x: Seq[(Char, Int)] =>
                    (itemWithIndex.foldLeft(true) {
                      case (accum, item) =>
                        accum && x.contains(item)
                    })
                  }) accum
              else accum + (itemMiddle -> (theseItems + itemWithIndex))
            }
            case None => accum + (itemMiddle -> Set(itemWithIndex))
          }
      }
      .toSeq
      .flatMap(_._2)
      .sortBy(r => (r(0)._2, r.last._2, r.length))
      .map { W =>
        (W.map(_._1).mkString, W(0)._2, W(1)._2 - 1 - W(0)._2)
      }
  }

  def challengeFunctionAlecFastINVALID(w: String): Seq[(String, Int, Int)] = {
    val length = w.length
    val wLower = w.toLowerCase

    (for {
      index <- 0 to length - 3

      jumpNum <- 1 until Math.ceil((length - index - 1) / 2).toInt + 1

      wordLength = Math.ceil((length - index) * 1d / jumpNum).toInt

      t <- Seq(0, 1)

      thisSampleChar <- 2 - t to (wordLength - t) / 2
      if (0 until thisSampleChar).forall { xx =>
        wLower(index + (thisSampleChar - xx - 1) * jumpNum) ==
          wLower(index + (thisSampleChar + t + xx) * jumpNum)
      }
    } yield {
      Seq.tabulate(thisSampleChar * 2 + t) { k =>
        index + jumpNum * k
      }
    }).foldLeft(Map[Double, Seq[Seq[Int]]]()) {
      (savedPalindromes: Map[Double, Seq[Seq[Int]]], testingPalindrome: Seq[Int]) =>
        val palindromeCentre = (testingPalindrome.head + testingPalindrome.last) * 0.5
        savedPalindromes.get(palindromeCentre) match {
          case Some(savedPalindromesWithSameCentre: Seq[Seq[Int]]) =>
            if (savedPalindromesWithSameCentre.exists { savedPalindrome: Seq[Int] =>
              //savedPalindrome.intersect(testingPalindrome).size == testingPalindrome.size // 172.5
              testingPalindrome.forall { item: Int =>
                savedPalindrome.contains(item)
              }
            }) savedPalindromes
            else savedPalindromes + (palindromeCentre -> (savedPalindromesWithSameCentre :+ testingPalindrome))
          case None => savedPalindromes + (palindromeCentre -> Seq(testingPalindrome))
        }
    }
      .values
      .toSeq
      .flatten
      .sortBy { r =>
        (r(0), r.last, r.size)
      }
      .map { W =>
        (W.map(wLower(_)).mkString, W(0), W(1) - 1 - W(0))
      }
  }

  def challengeFunctionAlecSmall(w: String): Seq[(String, Int, Int)] = {
    val N = w.indices

    (for {
      i <- N
      g <- N
      c = (i to N.last by g + 1).map {
        w(_).toLower
      }
      j <- 1 to c.length / 2
      (l, r) = c.mkString splitAt j
      s = l.reverse
      t <- 0 to 1
      if j > 1 - t & s == r.slice(t, t + j)
    } yield {
      c.take(2 * j + t).zipWithIndex.map { case (m, k) => (m, i + g * k + k) }
    }).foldLeft(Set[Seq[(Char, Int)]]())((a, i) =>
        if (a.exists { x =>
              i.toSet.subsetOf(x.toSet) & (i(0)._2 + i.last._2) == (x(0)._2 + x.last._2)
            }) a
        else a + i,
      )
      .toSeq
      .sortBy(r => (r(0)._2, r.last._2, r.length))
      .map { W =>
        (W.map(_._1).mkString, W(0)._2, W(1)._2 - 1 - W(0)._2)
      }
  } //446

}
