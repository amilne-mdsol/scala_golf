package example

import munit.FunSuite

class ExampleSpec extends FunSuite {

  val mainFunction = new MainAlec()
  //val mainFunction = new MainAlecSmall()

  test("challenge test easy 1") {
    assertEquals(mainFunction.challengeFunction("hannah"), Seq(("hannah", 0, 0)))
  }

  test("challenge test easy 2") {
    assertEquals(mainFunction.challengeFunction("hANaH"), Seq(("hanah", 0, 0)))
  }

  test("challenge test easy 3") {
    assertEquals(mainFunction.challengeFunction(""), Seq())
  }

  test("challenge test easy 4") {
    assertEquals(mainFunction.challengeFunction("Aa"), Seq())
  }

  test("challenge test easy 5") {
    assertEquals(mainFunction.challengeFunction("aab"), Seq())
  }

  test("challenge test 1") {
    assertEquals(mainFunction.challengeFunction("zzh11a22N33n44a55hyy"), Seq(("hannah", 2, 2), ("n33n", 8, 0)))
  }

  test("challenge test 2") {
    assertEquals(mainFunction.challengeFunction("zzh11a22n37n44a55hyy"), Seq(("hannah", 2, 2)))
  }

  test("challenge test 3") {
    assertEquals(
      mainFunction.challengeFunction("h11A11n22n22a33h"),
      Seq(("hannah", 0, 2), ("11a11", 1, 0), ("n22n", 6, 0), ("22n22", 7, 0)),
    )
  }

  test("challenge test 4") {
    assertEquals(
      mainFunction.challengeFunction("zzh11a27n44a55hyYpoiuytrewqlkjhgabbafdsa"),
      Seq(
        ("hanah", 2, 2),
        ("hyh", 2, 13),
        ("aua", 5, 14),
        ("ata", 5, 16),
        ("ara", 11, 11),
        ("awa", 11, 13),
        ("hth", 14, 7),
        ("yoy", 15, 2),
        ("abba", 32, 0),
        ("ada", 35, 1),
      ),
    )
  }

  test("challenge test 5") {
    assertEquals(
      mainFunction.challengeFunction("H11ag2n3an44a55hg667H"),
      Seq(("hannah", 0, 2), ("h4h", 0, 9), ("hgaagh", 0, 3), ("g4g", 4, 5), ("a4a", 8, 1)),
    )
  }

  test("challenge test 6") {
    assertEquals(
      mainFunction.challengeFunction("a11b22c33d4eef5f6eed7d88c99b00a"),
      Seq(("abcdefedcba", 0, 2), ("d5d", 9, 4), ("deffed", 9, 1), ("e5e", 11, 2), ("f5f", 13, 0), ("d7d", 19, 0)),
    )
  }

  test("challenge test 7") {
    assertEquals(
      mainFunction.challengeFunction("a1bbc2c3bda4e55f66e77d88c99b00a"),
      Seq(
        ("a2a", 0, 4),
        ("abccba", 0, 1),
        ("abcdefedcba", 0, 2),
        ("b2b", 2, 2),
        ("c2c", 4, 0),
        ("c5c", 4, 9),
        ("a7a", 10, 9),
      ),
    )
  }

  test("challenge test 8") {
    assertEquals(
      mainFunction.challengeFunction("abcdefgfedcbabcdefgf"),
      Seq(
        ("abcdefgfedcba", 0, 0),
        ("bfb", 1, 5),
        ("cec", 2, 5),
        ("ddd", 3, 5),
        ("ece", 4, 5),
        ("fbf", 5, 5),
        ("fgfedcbabcdefgf", 5, 0),
        ("fbf", 7, 5),
        ("fgf", 17, 0),
      ),
    )
  }

  test("challenge test 9") {
    assertEquals(
      mainFunction.challengeFunction("abahaba"),
      Seq(("aba", 0, 0), ("aaa", 0, 1), ("abahaba", 0, 0), ("aaa", 2, 1), ("aba", 4, 0)),
    )
  }

  test("challenge test 10") {
    assertEquals(
      mainFunction.challengeFunction("aqbraharbqa"),
      Seq(("aba", 0, 1), ("ara", 0, 2), ("aqbraharbqa", 0, 0), ("ara", 4, 2), ("aba", 6, 1)),
    )
  }

  test("challenge test 11") {
    assertEquals(
      mainFunction.challengeFunction("r11aqbraharbqa22r"),
      Seq(
        ("rar", 0, 2),
        ("rbr", 0, 4),
        ("rqhqr", 0, 3),
        ("aba", 3, 1),
        ("ara", 3, 2),
        ("aqbraharbqa", 3, 0),
        ("rbr", 6, 4),
        ("ara", 7, 2),
        ("aba", 9, 1),
        ("rar", 10, 2),
      ),
    )
  }

  test("challenge test 12") {
    assertEquals(
      mainFunction.challengeFunction("a123b4e5c6f7d8f9c0e1b234a"),
      Seq(("aedea", 0, 5), ("abcdcba", 0, 3), ("1f1", 1, 8), ("becfdfceb", 4, 1), ("4f4", 5, 8)),
    )
  }

  test("challenge test 13") {
    assertEquals(
      mainFunction.challengeFunction("aaaaaaaaaa"),
      Seq(
        ("aaa", 0, 0),
        ("aaaa", 0, 0),
        ("aaaaa", 0, 0),
        ("aaaaaa", 0, 0),
        ("aaaaaaa", 0, 0),
        ("aaaaaaaa", 0, 0),
        ("aaaaaaaaa", 0, 0),
        ("aaaaaaaaaa", 0, 0),
        ("aaaaaaaaa", 1, 0),
        ("aaaaaaaa", 2, 0),
        ("aaaaaaa", 3, 0),
        ("aaaaaa", 4, 0),
        ("aaaaa", 5, 0),
        ("aaaa", 6, 0),
        ("aaa", 7, 0),
      ),
    )
  }

  test("challenge test performance 400") {
    val result = mainFunction.challengeFunction(
      "bfIGfGlTyKfNJOWxYtAjPVSLrrokmqLMlJxjUaiKrrZXPWvjTMeYyPNTNEOFPnyvZNyfxiIlWGQeqYwcUlphROoWFeaLBntHsMnvJXwUpySDMCLqllzjWtUDIaipHaCNrPNKlCTzbzHWErUXkulqgrxqqxmLoPDLOiZJFXOtjsyEquiBFvIAiXUlXkLnMDqbKtpIlBkUkOaxLxxXNkSedTNYQbvqFBjnSxudGPFxhhTBnhOJoleSgrdcPlHFZMmQYKHjlXEHmZEaqnBZHgJgfCwMIbJBgkKxaosUVQYGexvzLFpPDuOvgOhTGLYFhUXMsakEpwglVTxWJmrgBDjHcbueUchSYUlpAGQxomBRavIDKDKUEphPrXQnbctNIFHwQoDYxoQQiQgTqWlL",
    )
    assertEquals(result.length, 1551)
    assertEquals(
      result.filter(_._1.length == 3).length,
      1483,
    )
    assertEquals(
      result.filter(_._1.length == 4).length,
      45,
    )
    assertEquals(
      result.filter(_._1.length == 5).length,
      20,
    )
    assertEquals(
      result.filter(_._1.length == 6).length,
      1,
    )
    assertEquals(
      result.filter(_._1.length == 7).length,
      2,
    )
    assertEquals(
      result.filter(_._1.length >= 8).length,
      0,
    )
  }

  test("challenge test performance 1000") {
    val result = mainFunction.challengeFunction(
      "yGyEjnhxJsfjqUFbmaATbKalWRexURzKIIgFzUXJNRVAUYcExiNUoRViqqDjAzBauPKEcwFoGUBgkWRnZnpKFCdSHLZtMnsCmHlldfAZzCRwYQGozuBiDZRmNsEGnLMjGddTSFHDntwFAcWtXgMEoeHsPWDAjHgoZwwVRESkgOyroUQXqjAVXcUxKSCokETJMHMMTYjFDwKenJqlzOZjRsFlZMVBcupAqANwyEOMEvRfeLbqpfLWizoClnBTlsmmTRaPscTsTmOmzHiPDGvcWBuhWORwLQjGqdKJHUHUmxdOSFEzPGfuSbkuPnuinuIxGeyqyJjAgPEszKKhQNqrqUsVtehRjRWggcOQXwolzkazkpaIwOtHAwylrFPsORtyEUwlxPUAWDYAxUOlujvbvYinxCWfvRRbwDYAnMCLVlVufoLnqYKKwPkxwVEZIlyrrSsjYqdauEdRthGkVimskzPbDhlhorWpJBcaEkOTWzQLgAcgNhUZsxcMecBVcJYEeyTKVxxgKUPemMqfhIzhVFFSanVSwpdPeesDSZhgoGspQmKGnncBoCVpocjVYstZHweHyaDvIxPArxSxYAWQhlwdajxKtXNiSsgEtwOrUrUtvTEqWtwGZLgvHezeAfaTCVsOylfgOacJjooxNnVhaNCnDHtzaXkQysXqAclPljuAgUgbZDmnErCySNhIEJoeLJvzKRpRdSuOhDjMUdvHqdVQYcCSFaQZMsnmUPkArWfqphXXeICaQKORrtlrUAlmqdQoydXLOCtahQcakfXESABxuOhyMGEduVNHDfXNSEwTgrvBSUrFlHZGIQSsznxuKlMSjaBLnQWrXniKzytpSSkrWHzAmhkHkZRpiDFgWruzYOEmfITWQgfuKnmNniZFNFpLYWgpdlumdgPtuDGRhtkwaPAxEMjAqiQhyqzmTpUVJyiDfbjmifNBErbewBoUDrgCvjQEGKLGnHPDlodNKudrrBMCTQbHfRSSyIwm",
    )
    assertEquals(result.length, 9632)
    assertEquals(
      result.filter(_._1.length == 3).length,
      9155,
    )
    assertEquals(
      result.filter(_._1.length == 4).length,
      256,
    )
    assertEquals(
      result.filter(_._1.length == 5).length,
      208,
    )
    assertEquals(
      result.filter(_._1.length == 6).length,
      6,
    )
    assertEquals(
      result.filter(_._1.length == 7).length,
      5,
    )
    assertEquals(
      result.filter(_._1.length == 8).length,
      1,
    )
    assertEquals(
      result.filter(_._1.length == 9).length,
      1,
    )
    assertEquals(
      result.filter(_._1.length >= 10).length,
      0,
    )
  }

  test("challenge test performance 2000") {
    val result = mainFunction.challengeFunction(
      "cHBnGgDWNrYYDhDtRbaGBIHgcvEPjwEJqvvLTdZSZXRGibGjMGDgeCsvIKNaKHZWGnnfUVGJQaWUGhLnSpxZVbGsxjVVUoKhbUNUqMWpfxAKpdsAKmFYfhFqmcDjfasovnqnPuWIaJvSLWVbkaDGJFlrWOnxeNyWsPmIcniAPcijsXYjHcmpzGpCnCqwifUTnQLKMMxdqdRtkIhwHbNUwMrAFjTdjgsLKCDeiMIHyClWtJCIBNIHCgnvRFAUsOqsgCaWDOAhuPLvDRQomRYVOvazHMdjiFIgjfGMVgjAMKkHOsAXcXgncAUFhdKkVgGNmLdqlGDabhCmQgdrJTVLstIYqOMChzhiKHHFOJxnVBcYuKJEKPQvdAgSPTBpfCFWhPHdBinmXcGbqaHlWGUrJkdtvbIeCSiTdJEVUGexlKzMEtKWgmTtRlTDPsvEvstwBZEajoqjWrUxHkEjisSmOgekCpPXexicUMOGFAYgbCFZNbxilickopwtoquTeozjiRuPMlcQMhBYMWNpaGZJFGKxPoiLPDJoEuHjQGCYMdlJijQgXswLOzStEjykVSOLfSgOOZORlwJfgUIAyMZNpJTgYVAmLqEDPJgEkPhcRUprwsHCNcHKLnpYjkFiniusfYkPiDNUVecSVnmgMjOYcIWGxzLbtshuZMkURNlrWITYcwZUJwBYUtycUDAdpxmupNhSMwwtBuWdmeHCmZucZIIldnxzDJnOoZYImjorqISGevLTprBVtutuMzoVBDEnbicCwUxowUYvfhhkPZlGUrIpQDyrqsuZGsUHXvUrzpsXqwhFjYvxbEFyeGAichPXSCcHCnEzQyJXAOcunWXzHoEDEwvBFtCPuvSAKUKwBhlWjpoviSYABVFbBINHqodiACQkuaFPIIZMdapJVnoqtqKdJzFzakYmQFRHjWjScSqcPTHQGFWhpKoeODOyCxHAFSZFfgnhWTIpbmrbVpsGleCKBsJNIZLOlxQvMqThyMFBUdeIvLELsuZwuTycpBljDSXusTlnDkJQWAkCtWYInzhNSCMEwoNyjGenTCKXqUhxnpbHUbQTmIAnoullWMiPKyXBBWdByUMzfOKgTbnpJMVztbfFdZKHhCdZaqZPaiWuhgElGQSFZdDTuZrJIZNmIfNdmylDhPsSryymQZMQIhNLfCCWAPbWdFEZIgxsBOrsXhNPbZJigCdJCNvoukeLOvgYZmWSKCQbqBGIwkNHqzbNBePnPjgpkCdPJWFwfJwUelgirnWxokasGxqMYqwWbtHlKEvYWbDoDUtJTeGgMqgSTvNqsJtVKFLMVNivIUGpQvIiMCjmEeIEcuWiwVIngFtNwOfMotObYZkcyBXOoiDXmnvPKGpoagKzOPDImCZhHQaqHRcqOHpTMDVqaxYQOjmFOKoBbEZHpfLxIzcYMjpfKcGZXZsxBhAKmHxxnbpoEQquMVfwZsUiFGuwplzTZKsCsyhvjuquotOZbOdxmIQktGTDKkPTLmvDbITbOLerkrkJVgwQhDwMSCmzIHAytxokqBQVgYPIdOPDYZJkawyCkIBssIOZeCosevwdUBdpcwnAdCysRvyHhvdiipRYrYvjaJoDNDOfsmyRjEVfNhgFOaRlBTkoROTehKOGPqPMZnFxBAchvXyofxvyQofOdyvHOegnvmcggLFXfYKIqwruCwmcMnPzOVIhDteuqCQszAPGCzLBLtiYQTxWAnjGVYtNFvPkPcqEDRgtZAXrEuSbHEiRpnenQLXVksOqAtcTqXDazgsBkdnBFsJsWmWshNYCongMgAthDAGyZsodgixNJrKMVKLbHFRzUhJSYLMygxZvfMQuYeVrKzgaFEsirQlYLEjNXxiIYxifUxLXCtwPPNUDGjUQvqmgRdQLxgEcSTGMaiCVdVRDzUUiNIuGDACgrgAoVsSBZNLrekQyUVBTSvigCNDkkRngFjcjVUAEtURzkAamMjayOnwYQQVkFQKZtPAjABxctDeO",
    )
    assertEquals(result.length, 38813)
    assertEquals(
      result.filter(_._1.length == 3).length,
      37093,
    )
    assertEquals(
      result.filter(_._1.length == 4).length,
      963,
    )
    assertEquals(
      result.filter(_._1.length == 5).length,
      722,
    )
    assertEquals(
      result.filter(_._1.length == 6).length,
      19,
    )
    assertEquals(
      result.filter(_._1.length == 7).length,
      16,
    )
    assertEquals(
      result.filter(_._1.length >= 8).length,
      0,
    )
  }
}
