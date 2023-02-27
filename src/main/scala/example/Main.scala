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
    Seq.empty[(String, Int, Int)]
  }
}
