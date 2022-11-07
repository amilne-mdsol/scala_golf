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

  def challengeFunction(inputSeq: Seq[Int]): Seq[(Int, Int, Int)] = {
    Seq((0, 0, 0))
  }
}
