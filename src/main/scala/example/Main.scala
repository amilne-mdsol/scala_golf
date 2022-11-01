package example

object Main {

  def main(args: Array[String]): Unit =
    println("Hello com.example.Empty Project!")

  /*
  SCALA GOLF CHALLENGE #1
  Write a function that when passed a string will return a true only if all the first instances of a double letter combination are in reverse alphabetical order

  e.g.
  input - azzbmmzdd -> true as the double letters zz, mm and dd are in z->a order.
  input - azzbmmzziee -> true as the second zz has already occurred, so can be ignored
  input - aabbrr -> false as the double letters are not in z->a order

  There are a series of unit tests in this project that need to all pass for your submission to be considered.

  You will be scored for the least number of characters you use in your function(s). White space, carriage returns, etc, all count.

  Please use the function challengeFunction below.
   */

  def challengeFunctionTom(a: String) = {
    var l = -1;
    for {
      p <- Range(122, 97, -1)
      i = a.toLowerCase.indexOf(p.toChar.toString * 2)
    } yield if (i >= 0) if (i > l) l = i else return false;
    true
  }
  //186 characters

  def challengeFunctionLuke(s: String) = {
    val f = s.zip(s.tail).filter(x => x._1 == x._2).distinct; f == f.sortBy(-_._1)
  }
  //103 characters

  def challengeFunctionAmir(inputString: String): Boolean = {
    if (inputString.isEmpty) true
    else if (".*(.)\\1.*".r.matches(inputString)) {
      val s1 = inputString.toList.zip(inputString.drop(1)).filter(c => c._1 == c._2).distinct.map(_._1)
      s1.reverse == s1.reverse.sorted
    } else true
  }
  //272 characters

  def challengeFunctionAlec(inputString: String): Boolean = {
    val brokenDownInputStringWithIndex: Seq[(Char, Int)] = inputString.toCharArray.toSeq.zipWithIndex
    val brokenDownInputStringWithIndexWithoutLast = brokenDownInputStringWithIndex.dropRight(1)
    val onlyDuplicatedCharactersInBrokenDownInputStringWithIndexSeqSeq: Seq[Seq[Char]] =
      brokenDownInputStringWithIndexWithoutLast.map {
        case (thisValue, index) =>
          val nextValue = brokenDownInputStringWithIndex.apply(index + 1)._1
          if (thisValue == nextValue) {
            Seq(thisValue)
          } else {
            Seq.empty
          }
      }
    val onlyDuplicatedCharactersInBrokenDownInputStringWithIndex: Seq[(Char, Int)] =
      onlyDuplicatedCharactersInBrokenDownInputStringWithIndexSeqSeq.flatten.zipWithIndex
    val inputStringWithoutDuplicatesReduceDoublesToSinglesSeqSeq: Seq[Seq[Char]] =
      onlyDuplicatedCharactersInBrokenDownInputStringWithIndex.map {
        case (thisValue, index) =>
          val listBefore: Seq[Char] = onlyDuplicatedCharactersInBrokenDownInputStringWithIndex.take(index).map(_._1)
          val howManyBeforeThisChar = listBefore.filter(_ == thisValue)
          if (howManyBeforeThisChar.length > 0) {
            Seq.empty
          } else {
            Seq(thisValue)
          }
      }
    val inputStringWithoutDuplicatesReduceDoublesToSingles =
      inputStringWithoutDuplicatesReduceDoublesToSinglesSeqSeq.flatten
    val inputStringWithoutDuplicatesReduceDoublesToSinglesSortedReverse =
      inputStringWithoutDuplicatesReduceDoublesToSingles.sorted.reverse
    inputStringWithoutDuplicatesReduceDoublesToSingles == inputStringWithoutDuplicatesReduceDoublesToSinglesSortedReverse
  }
  //1472 characters

  def challengeFunction(a: String) = {
    val b = a.toSeq.zipWithIndex
    val c =
      b.dropRight(1)
        .flatMap {
          case (v, i) =>
            if (v == b.apply(i + 1)._1) {
              Seq(v)
            } else {
              Seq.empty
            }
        }
        .zipWithIndex
    val d =
      c.flatMap {
        case (v, i) =>
          if (c.take(i).map(_._1).contains(v)) {
            Seq.empty
          } else {
            Seq(v)
          }
      }
    d == d.sorted.reverse
  }
  //276 characters

}
