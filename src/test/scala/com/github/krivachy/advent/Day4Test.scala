package com.github.krivachy.advent

import org.scalatest.{Matchers, WordSpec}

class Day4Test extends WordSpec with Matchers {

  "The Day 4 part 1 solution" should {
    "pass the first example" in {
      Day4.solutionPart1(Seq(Seq("aa", "bb", "cc", "dd", "ee"))) shouldBe 1
    }
    "pass the second example" in {
      Day4.solutionPart1(Seq(Seq("aa", "bb", "cc", "dd", "aa"))) shouldBe 0
    }
    "pass the third example" in {
      Day4.solutionPart1(Seq(Seq("aa", "bb", "cc", "dd", "aaa"))) shouldBe 1
    }
  }

  "The Day 4 part 2 solution" should {
    "pass the first example" in {
      Day4.solutionPart2(Seq(Seq("abcde", "fghij"))) shouldBe 1
    }
    "pass the second example" in {
      Day4.solutionPart2(Seq(Seq("abcde", "xyz", "ecdab"))) shouldBe 0
    }
    "pass the third example" in {
      Day4.solutionPart2(Seq(Seq("a", "ab", "abc", "abd", "abf", "abj"))) shouldBe 1
    }
    "pass the fourth example" in {
      Day4.solutionPart2(Seq(Seq("iiii", "oiii", "ooii", "oooi", "oooo"))) shouldBe 1
    }
    "pass the fifth example" in {
      Day4.solutionPart2(Seq(Seq("oiii", "ioii", "iioi", "iiio"))) shouldBe 0
    }
  }
}
