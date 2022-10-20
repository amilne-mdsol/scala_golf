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

  def challengeFunction(inputString: String): Boolean = {
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
}
