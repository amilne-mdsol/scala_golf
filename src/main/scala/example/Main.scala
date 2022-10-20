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
    true
  }
}
