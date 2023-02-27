package example

import scala.collection.MapView

object Main {

  /*
    Challenge 8
    Keyboard distance

    Given a string representing array of characters, give the minimum overall distance a finger would have to travel to type in the given word.

    Note: There can be duplicate letters in the keyboard layout.
    You will need to find the shortest total distance taking these multiple options into consideration.

    This challenge has two ways to win

    1 - Shortest code solution. Use the fewest non-space characters possible to fulfill all the test scenarios.
    2 - Fastest code solution. Write the most clock-cycle efficient method for solving this problem.

    Notes:
    The function declaration is not to be altered. These characters will not be added to the character count.
    Round the result to a 1 decimal place Double
    Keys are 1 unit square and there is no gap between keys.
    If the input word uses a letter that isn't in the keyboard then return -1.0 for the result
    No Variables, this is Scala, we use Values.

    Example and explanation:

    challenge test 1
    assertEquals(mainFunction.challengeFunction("""qwertyuiop
      |asdfghjkl
      |zxcvbnm""".stripMargin, "inputword"), 28.7)

    This means that the keyboard would be like this:
    qwertyuiop
    asdfghjkl-
    zxcvbnm---

    Which is just a slightly shifted version of the regular English language keyboard

    Imagine you are typing with just one finger going from letter to letter in a perfectly straight line

    If the word was "it"
    You would start with your finger above the "i"
    Move to the left 3 buttons to the "t" letter
    This is a distance of 3, so the output from this keyboard for the word "it" would be 3.0

    Typing the word "inputword" is a bit longer, but just the same calculation for each letter.

    Moving from letter to letter in a big zigzag pattern for the whole word results in a distance of 28.7 (rounded to 1dp)

    Here is the input keyboard with all the unused letters removed for display purposes.
    -w-rt-uiop
    --d-------
    -----n----

    i -> n = down 2 left 2 (sqrt(2^2 + 2^2)) approx 2.8
    n -> p = up 2 right 4 approx 4.5
    p -> u = left 3
    u -> t = left 2
    t -> w = left 3
    w -> o = right 7
    o -> r = left 5
    r -> d = down 1 left 1 approx 1.4

    Adding all these together equal 28.7

    In the second test there is another "w" letter that results in a shorter distance overall,
    so the second test has a very similar keyboard design (with the extra w at the end) but a shorter distance of 24.6

   */

  def challengeFunction(k: String, w: String): Double = {
    //challengeFunctionAlecFast(k, w)
    //challengeFunctionAlecShort(k, w)
    //challengeFunctionBen(k,w)
    challengeFunctionBenShortNOTVALID(k, w)
    //challengeFunctionLukeShortNOTVALID(k, w)
    //challengeFunctionLukeShort(k, w)
    //challengeFunctionLukeFast(k, w)
    //challengeFunctionJitendraShort(k,w)
    //challengeFunctionJitendraFast(k,w)
    //challengeFunctionDilyanSmall(k, w)

    //challengeFunctionTomFast(k,w)
  }

  def diffCalc(a: (Int, Int), b: (Int, Int)): Double = {
    val xDiff = a._1 - b._1
    val yDiff = a._2 - b._2
    val xSquared = 1.0 * xDiff * xDiff
    val ySquared = 1.0 * yDiff * yDiff

    Math.sqrt(xSquared + ySquared)
  }

  case class PathClass(startChar: Char, startPos: (Int, Int), endChar: Char, endPos: (Int, Int), dist: Double)

  def challengeFunctionAlecFast(k: String, w: String): Double = {
    val keyboardSplit: Seq[(String, Int)] = k.split('\n').zipWithIndex

    val keyboardArray: Seq[(Char, (Int, Int))] = keyboardSplit.flatMap {
      case (str, i) =>
        str.zipWithIndex.map { case (a: Char, b: Int) => (a, (b, i)) }
    }

    val wLower = w.toLowerCase
    val wPaired = wLower.zip(wLower.tail)
    val wParsedSeq = wPaired.flatMap {
      case (beforeLetter, afterLetter) =>
        if (beforeLetter == afterLetter) {
          None
        } else {
          Some(beforeLetter)
        }
    } ++ Seq(wLower.last)

    val letters = wLower.distinct
    val letterPositions: Seq[(Char, Seq[(Int, Int)])] = letters.map { letter: Char =>
      val things: Seq[(Char, (Int, Int))] = keyboardArray.filter(_._1 == letter)
      val things2: Seq[(Int, Int)] = things.map(x => x._2)
      (letter, things2)
    }

    val letterPositionsMap: Map[Char, Seq[(Int, Int)]] = letterPositions.toMap

    val wordPathToUse: Seq[(Char, Char)] = if (wParsedSeq.length > 1) {
      wParsedSeq.sliding(2).toSeq.map(x => (x(0), x(1)))
    } else {
      Seq()
    }

    val wordPathToUseSet = wordPathToUse.toSet

    val allPairs2: Map[Int, Map[(Int, Int), Seq[PathClass]]] = wordPathToUseSet.toSeq.map {
      case (startChar: Char, endChar: Char) =>
        val startOptions = letterPositionsMap(startChar)
        val endOptions = letterPositionsMap(endChar)
        val temp: Map[(Int, Int), Seq[PathClass]] = startOptions.map { startPos: (Int, Int) =>
          (
            startPos,
            endOptions
              .map { endPos: (Int, Int) =>
                PathClass(startChar, startPos, endChar, endPos, diffCalc(startPos, endPos))
              }
              .sortBy(_.dist),
          )
        }.toMap
        (startChar * 1000 + endChar, temp)
    }.toMap

    def tempRecur(
      startPosOpt: Option[(Int, Int)],
      startChar: Char,
      remainingWord: Seq[Char],
      dist: Double,
      lowestDistOpt: Double,
    ): Double = {
      if (remainingWord.length > 0) {
        val nextLetter = remainingWord(0)

        val foundOptions: Seq[PathClass] = if (startPosOpt.isEmpty) {
          allPairs2(startChar * 1000 + nextLetter).flatMap(_._2).toSeq
        } else if ((lowestDistOpt - (dist + 1.0 * (remainingWord.length - 1))) >= 1.0) {
          allPairs2(startChar * 1000 + nextLetter)(startPosOpt.get)
        } else {
          Seq()
        }

        var lowestDistSoFarFold = lowestDistOpt

        foundOptions.foreach { foundOption: PathClass =>
          val newDist = dist + foundOption.dist
          if (newDist < lowestDistSoFarFold) {
            val newLowestDist = tempRecur(
              Some(foundOption.endPos),
              nextLetter,
              remainingWord.tail,
              newDist,
              lowestDistSoFarFold,
            )
            if (newLowestDist < lowestDistSoFarFold) {
              lowestDistSoFarFold = newLowestDist
            }
          }
        }

        lowestDistSoFarFold
      } else {
        dist
      }
    }

    val total = tempRecur(None, wParsedSeq.head, wParsedSeq.tail, 0.0, Double.MaxValue) //.getOrElse(0.0)

    val totalParsed = if (total == Double.MaxValue) -1.0 else total

    val totalMult = totalParsed * 10
    val totalMultInt = totalMult.round

    (1.0 * totalMultInt) / 10
  }

  def challengeFunctionAlecShortNOTVALID(k: String, w: String): Double = {
    def t(P: (Int, Int), D: Double, W: String): Double =
      try k
        .split('\n')
        .zipWithIndex
        .flatMap {
          case (f, h) =>
            f.zipWithIndex
              .filter(_._1 == W.head)
              .map { g =>
                t((g._2, h), if (P._1 < 0) D else D + math.hypot(P._1 - g._2, P._2 - h), W.tail)
              }
        }
        .minOption
        .getOrElse(-1)
      catch {
        case _ => D
      }

    (t((-1, 0), 0, w.toLowerCase) * 10).round * 1.0 / 10
  } //283

  def challengeFunctionAlecShort(k: String, w: String): Double = {
    def t(P: (Int, Int), D: Double, W: String): Double =
      try k
        .split('\n')
        .zipWithIndex
        .flatMap {
          case (f, h) =>
            f.zipWithIndex
              .filter(_._1 == W.head)
              .map { g =>
                val (a, b) = (P._1 - g._2, P._2 - h)
                t((g._2, h), if (P._1 < 0) D else D + Math.sqrt(a * a + b * b), W.tail)
              }
        }
        .minOption
        .getOrElse(-1)
      catch { case _ => D }

    (t((-1, 0), 0, w.toLowerCase) * 10).round * 1.0 / 10
  } //300

  def challengeFunctionBenShortNOTVALID(k: String, w: String): Double = {
    try {
      var t: Seq[((Double, Double), Double)] = Seq()

      w.toLowerCase.foreach { c =>
        t = k.toLowerCase
          .split("\n")
          .zipWithIndex
          .flatMap { j =>
            j._1.zipWithIndex.filter(_._1 == c).map { C =>
              val N = (C._2 * 1.0, j._2 * 1.0)
              (
                N,
                t.map { n =>
                    val (c, d) = (N._1 - n._1._1, N._2 - n._1._2)
                    n._2 + math.sqrt(c * c + d * d)
                  }
                  .minOption
                  .getOrElse(0.0),
              )
            }
          }
        t.head
      }
      (t.map(_._2).min * 10).round * 1.0 / 10
    } catch {
      case _ => -1.0
    }
  } //345

  def challengeFunctionBen(k: String, w: String): Double = {

    type V = (Double, Double)

    def distance(a: V, b: V): Double = {
      val dx = a._1 - b._1
      val dy = a._2 - b._2
      math.sqrt(dx * dx + dy * dy)
    }

    // No var solution, but it forces lots of boxing/unboxing of Doubles
    //  def f(n: Vector[(V, Double)], v: V): Double = n.map(n => n._2 + distance(v, n._1)).min

    def f2(n: Vector[(V, Double)], v: V): Double = {
      var i = 0
      var d = Double.MaxValue
      while (i < n.length) {
        val node = n(i)
        val candidateDistance = node._2 + distance(v, node._1)
        if (candidateDistance < d) {
          d = candidateDistance
        }
        i += 1
      }
      d
    }

    // Remove consecutive duplicate characters in the input word
    // e.g. inpuutwoooord => inputword
    // Not ideal for long words (O(n^2) and there is w2.contains in a loop below), but there aren't any long words :D
    val w2 = w.toLowerCase.foldLeft("")((prev, c) => if (prev.isEmpty || c != prev.last) prev.appended(c) else prev)
    val k2 = k.toLowerCase

    // Part 1: Build a map of the characters to the list of all their coordinates.
    // Performance of this part is basically irrelevant.
    //
    // e.g.
    //
    //  abc        a -> (0,0), (1,0), (1,1), (1,2), (2,1)
    //  aaa =>     b -> (0,1), (2,0), (2,2)
    //  bab        c -> (0,2)

    val keymap = collection.mutable.Map[Char, Vector[V]]()

    k2.split("\n")
      .toList
      .zipWithIndex
      .flatMap { case (line, j) => line.zipWithIndex.map { case (c, i) => (c, (i.toDouble, j.toDouble)) } }
      .filter { case (c, _) => w2.contains(c) }
      .foreach { case (c, v) => keymap.updateWith(c)(vsOpt => vsOpt.map(vs => vs.appended(v)).orElse(Some(Vector(v)))) }

    scala.util
      .Try {

        // Part 2: Build a layered, directed graph where the layers are fully connected (AKA neural network).

        // Evaluate each layer in turn.
        // Examine every edge between this layer and the previous.
        // Save the shortest path length needed to reach each node.
        // Once you are at a node, the path behind it is irrelevant.
        // Solution is O(D^2) where D is max number of duplicate keys on the keyboard.

        val firstNodeListWithDistances: Vector[(V, Double)] = keymap(w2.head).map(n => (n, 0.0))

        val lastNodeListWithDistances: Vector[(V, Double)] = w2
          .drop(1)
          .foldLeft(firstNodeListWithDistances)((prevNode, c) => keymap(c).map(n => (n, f2(prevNode, n))))

        val result = lastNodeListWithDistances.map(_._2).min
        BigDecimal(result).setScale(1, BigDecimal.RoundingMode.HALF_UP).toDouble
      }
      .getOrElse(-1.0)
  }

  def challengeFunctionLukeShortNOTVALID(k: String, w: String): Double = {
    val c = k split '\n'

    def f(s: String, a: (Int, Int) = null): Double =
      if (s == "") 0
      else
        (for {
          i <- c.indices
          j <- c(i).indices
          if c(i)(j) == s(0)
        } yield (if (a == null) 0 else Math.hypot(i - a._1, j - a._2)) + f(s.tail, (i, j))).min

    try (f(w.toLowerCase) * 10).round / 10d
    catch _ => -1
  } //229

  def challengeFunctionLukeShort(k: String, w: String): Double = {
    import Math._
    val m = w.toLowerCase
    val c = k split '\n'

    def t(y: Any) =
      for {
        i <- c.indices
        j <- c(i).indices
        if c(i)(j) == y
      } yield (i, j)

    def f(s: String, a: (Int, Int) = null): Double =
      if (s.size < 1)
        0
      else
        t(s(0))
          .map(v =>
            (
              if (a == null) 0
              else sqrt(pow(v._1 - a._1, 2) + pow(v._2 - a._2, 2))
            ) + f(s.tail, v),
          )
          .min

    if (m forall (k contains _))
      round(f(m) * 10) / 10.0
    else
      -1
  } // 304

  def challengeFunctionLukeFast(k: String, w: String): Double = {
    import Math._
    import scala.concurrent.{Future, Await}
    import scala.concurrent.duration.Duration
    import scala.concurrent.ExecutionContext.Implicits.global

    if (w.length > 1) {
      val wLower = w.toLowerCase
      val wNoRepeats = wLower.head + wLower.sliding(2).collect { case v if v(0) != v(1) => v(1) }.mkString

      val c = k.split('\n')
      val coords = (
        for {
          i <- c.indices
          j <- c(i).indices
        } yield (c(i)(j), (i, j))
      ).groupMap(_._1)(_._2)

      if (wNoRepeats.forall(k contains _)) {
        var shortest = Double.MaxValue

        def findShortest(s: String, a: (Int, Int), currentLength: Double): Unit = {
          if (s.length < 1) {
            shortest = currentLength
          } else
            for {
              (p, q) <- coords(s.head)
              xDist = p - a._1
              yDist = q - a._2
              newLength = currentLength + sqrt((xDist * xDist) + (yDist * yDist))
              if newLength < shortest
            } findShortest(s.tail, (p, q), newLength)
        }

        val c = coords(wNoRepeats.head)
        val threads = max(1, c.length / Runtime.getRuntime.availableProcessors())

        val futures = Future.sequence(
          c.grouped(threads)
            .map(f =>
              Future {
                f.foreach(v => findShortest(wNoRepeats.tail, v, 0))
              },
            ),
        )

        Await.ready(futures, Duration.Inf)

        round(shortest * 10) / 10.0
      } else
        -1
    } else 0
  }

  def challengeFunctionJitendraShort(k: String, w: String): Double = {
    import java.lang.Math.{abs, sqrt}
    import scala.util.control.Exception.catching
    val rowMove: Array[Int] = Array(0, -1, 0, 1)
    val colMove: Array[Int] = Array(-1, 0, 1, 0)
    val strSeq = k.stripMargin.lines().toArray().toList.map(_.toString)
    val (row, col) = (strSeq.length, strSeq.maxBy(_.length).length)
    val arrStr = strSeq.map(str => str.toCharArray).toArray
    val arr = Array.tabulate(row, col)((i, j) =>
      (catching(classOf[IndexOutOfBoundsException]) either arrStr(i)(j)).toOption.getOrElse('-'),
    )

    def find(
      A: Array[Array[Char]],
      V: Array[Array[Boolean]],
      i: Int,
      j: Int,
      c: Char,
      R: Seq[(Int, Int)],
    ): Seq[(Int, Int)] = {
      if (i < 0 || j < 0 || i > A.length - 1 || j > A(0).length - 1 || V(i)(j)) return R
      V(i)(j) = true
      val updatedResult = if (A(i)(j).toString.equalsIgnoreCase(c.toString)) R :+ (i, j) else R
      rowMove.indices.flatMap(idx => {
        find(A, V, i + rowMove(idx), j + colMove(idx), c, updatedResult)
      })
    }

    val path = w
      .foldLeft(Seq(Seq((0, 0)))) { (acc, char) =>
        val nextElem = acc.last.flatMap {
          case (x, y) => find(arr, Array.ofDim[Boolean](row, col), x, y, char, Seq())
        }.distinct
        if (nextElem.isEmpty) return -1.0
        acc :+ nextElem
      }
      .drop(1)

    def findMinPath(startPoint: (Int, Int), pathList: Seq[Seq[(Int, Int)]], minDist: Double): Double = {
      if (pathList.isEmpty) {
        minDist
      } else {
        val (p1X, p1Y) = startPoint
        pathList.head.map {
          case (p2X, p2Y) =>
            val dist = minDist + sqrt(abs(p1X - p2X) * abs(p1X - p2X) + abs(p1Y - p2Y) * abs(p1Y - p2Y))
            findMinPath((p2X, p2Y), pathList.drop(1), dist)
        }.min
      }
    }

    val result = path.head.map(findMinPath(_, path.drop(1), 0.0)).min
    BigDecimal(result).setScale(1, BigDecimal.RoundingMode.HALF_UP).toDouble
  } //1430

  def challengeFunctionJitendraFast(k: String, w: String): Double = {
    import java.lang.Math.{abs, sqrt}
    import scala.collection.mutable
    import scala.util.control.Exception.catching
    val cache: mutable.Map[(Int, Int, Int), Double] = scala.collection.mutable.Map()
    val rowMove: Array[Int] = Array(0, -1, 0, 1)
    val colMove: Array[Int] = Array(-1, 0, 1, 0)
    val strSeq = k.stripMargin.lines().toArray().toList.map(_.toString)
    val (row, col) = (strSeq.length, strSeq.maxBy(_.length).length)
    val arrStr = strSeq.map(str => str.toCharArray).toArray
    val arr = Array.tabulate(row, col)((i, j) =>
      (catching(classOf[IndexOutOfBoundsException]) either arrStr(i)(j)).toOption.getOrElse('-'),
    )

    def find(
              A: Array[Array[Char]],
              V: Array[Array[Boolean]],
              i: Int,
              j: Int,
              c: Char,
              R: Seq[(Int, Int)],
            ): Seq[(Int, Int)] = {
      if (i < 0 || j < 0 || i > A.length - 1 || j > A(0).length - 1 || V(i)(j)) return R
      V(i)(j) = true
      val updatedResult = if (A(i)(j).toString.equalsIgnoreCase(c.toString)) R :+ (i, j) else R
      rowMove.indices.flatMap(idx => {
        find(A, V, i + rowMove(idx), j + colMove(idx), c, updatedResult)
      })
    }

    val path: Seq[(Seq[(Int, Int)], Int)] = w
      .foldLeft(Seq(Seq((0, 0)))) { (acc, char) =>
        val nextElem = acc.last.flatMap {
          case (x, y) => find(arr, Array.ofDim[Boolean](row, col), x, y, char, Seq())
        }.distinct
        if (nextElem.isEmpty) return -1.0
        acc :+ nextElem
      }
      .drop(1)
      .zipWithIndex

    def findMinPath(startPoint: (Int, Int, Int), pathList: Seq[(Seq[(Int, Int)], Int)]): Double = {
      // Base case, when we reach at the end.
      if (pathList.isEmpty) {
        0.0
      } else {

        val (p1X, p1Y, position) = startPoint
        //already calculated from start point, no need to redo
        if (cache.contains((p1X, p1Y, position))) cache((p1X, p1Y, position))
        else {
          pathList.head._1.map {
            case (p2X, p2Y) =>
              val tempDist = findMinPath((p2X, p2Y, pathList.head._2), pathList.drop(1))
              val dist = tempDist + sqrt(abs(p1X - p2X) * abs(p1X - p2X) + abs(p1Y - p2Y) * abs(p1Y - p2Y))
              cache.addOne((p1X, p1Y, pathList.head._2), dist)
              cache((p1X, p1Y, pathList.head._2))
          }.min
        }
      }
    }

    val result = path.head._1.map { start =>
      findMinPath((start._1, start._2, path.head._2), path.drop(1))
    }.min
    BigDecimal(result).setScale(1, BigDecimal.RoundingMode.HALF_UP).toDouble
  }

  def challengeFunctionDilyanSmall(k: String, w: String): Double = {
    val b = k
      .split("\n")
      .zipWithIndex
      .flatMap(r => r._1.zipWithIndex.map(c => (c._1, (c._2, r._2))))
      .groupBy(_._1)
      .mapValues(_.map(_._2))

    def t(a: Double, c: String, f: (Int, Int)): Double =
      if (c.isEmpty) a
      else b.getOrElse(c.head.toLower, return -1).map(l => t(a + math.hypot(l._1 - f._1, l._2 - f._2), c.tail, l)).min

    b.getOrElse(w.head.toLower, return -1).map(c => math.round(t(0, w.tail, c) * 10) / 10d).min
  } //361

  def challengeFunctionTomFast(k: String, w: String): Double = {
    import scala.annotation.tailrec
    import scala.collection.mutable
    import scala.collection.mutable.ListBuffer
    import scala.math.BigDecimal.RoundingMode.HALF_UP

    val lowerCaseWord: Array[Byte] = w.toLowerCase.getBytes

    type Coordinate = (Byte, Byte)
    type CurrentCoordinateAndLength = (Coordinate, Double)

    val keyLocationMap: mutable.HashMap[Byte, Vector[Coordinate]] = mutable.HashMap.empty

    val keyboardRows = k.toLowerCase
      .split("\n")

    val keyboardRowsWithoutSpaces = keyboardRows.map(r =>
      (r, r.flatMap {
        case c if c == ' ' => None
        case c => Some(c)
      }),
    )
    val removedSameRows = keyboardRowsWithoutSpaces
      .flatMap {
        case (r, rWithoutSpaces) if keyboardRowsWithoutSpaces.exists {
          case (r2, rWithoutSpaces2) if rWithoutSpaces2 == rWithoutSpaces && r2.length < r.length => true
          case _ => false
        } =>
          None
        case r => Some(r)
      }
      .map(_._1)

    //    println(removedSameRows.toList)

    removedSameRows
      .map(_.getBytes.zipWithIndex)
      .zipWithIndex
      .foreach {
        case (row, y) =>
          row.foreach {
            case (char, x) if (lowerCaseWord.contains(char)) =>
              keyLocationMap
                .get(char)
                .fold {
                  keyLocationMap.addOne(char -> Vector((x.toByte, y.toByte)))
                  ()
                }(locations => keyLocationMap.update(char, locations :+ (x.toByte, y.toByte)))
            case _ => ()
          }
      }

    val distanceMap: mutable.HashMap[(Coordinate, Coordinate), Double] = mutable.HashMap.empty

    @inline
    def distBetweenVectors(a: Coordinate, b: Coordinate): Double =
      distanceMap.getOrElseUpdate(
        (a, b),
        Math.abs(
          Math.sqrt(Math.pow(Math.abs(a._1 - b._1), 2) + Math.pow(Math.abs(a._2 - b._2), 2)),
        ),
      )

    val stringAsLocations: List[Vector[Coordinate]] =
      lowerCaseWord.map(c => keyLocationMap.getOrElse(c, Vector.empty)).toList

    val startingOptions: mutable.HashSet[CurrentCoordinateAndLength] =
      mutable.HashSet.from(stringAsLocations.head.map(c => (c, 0.0)))

    @tailrec
    def calc(
              stringAsLocations: List[Vector[Coordinate]],
              current: mutable.HashSet[CurrentCoordinateAndLength],
            ): mutable.HashSet[CurrentCoordinateAndLength] = {
      def innerCalc(coordinateOptions: Vector[Coordinate]): mutable.HashSet[CurrentCoordinateAndLength] = {

        val newAcc: mutable.HashSet[CurrentCoordinateAndLength] = mutable.HashSet.empty[CurrentCoordinateAndLength]
        current.foreach { current =>
          coordinateOptions.foreach { option =>
            newAcc.add(
              (
                option,
                current._2 + distBetweenVectors(current._1, option),
              ),
            )
          }
        }
        if (coordinateOptions.size == 1 && newAcc.nonEmpty) {
          mutable.HashSet(newAcc.minBy { case (_, length) => length })
        } else newAcc
      }

      stringAsLocations match {
        case Nil => current
        case coordinateOptions :: Nil =>
          innerCalc(coordinateOptions)
        case coordinateOptions :: remainingWord =>
          calc(remainingWord, innerCalc(coordinateOptions))
      }

    }

    val routes = calc(stringAsLocations.tail, startingOptions)

    if (routes.nonEmpty) {
      var minimumRoute: Double = Double.MaxValue
      routes.foreach { res =>
        if (res._2 < minimumRoute) minimumRoute = res._2
      }

      BigDecimal(minimumRoute).setScale(1, HALF_UP).toDouble
    } else -1.0

  }

}
