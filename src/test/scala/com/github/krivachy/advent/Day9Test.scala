package com.github.krivachy.advent

import org.scalatest.{Matchers, WordSpec}

class Day9Test extends WordSpec with Matchers {

  "The Day 9 part 1 solution" should {
    "pass the first example" in {
      Day9.solutionPart1("{}".toCharArray.toList) shouldBe 1
    }
    "pass the second example" in {
      Day9.solutionPart1("{{{}}}".toCharArray.toList) shouldBe 6
    }
    "pass the third example" in {
      Day9.solutionPart1("{{},{}}".toCharArray.toList) shouldBe 5
    }
    "pass the fourth example" in {
      Day9.solutionPart1("{{{},{},{{}}}}".toCharArray.toList) shouldBe 16
    }
    "pass the fifth example" in {
      Day9.solutionPart1("{<a>,<a>,<a>,<a>}".toCharArray.toList) shouldBe 1
    }
    "pass the sixth example" in {
      Day9.solutionPart1("{{<ab>},{<ab>},{<ab>},{<ab>}}".toCharArray.toList) shouldBe 9
    }
    "pass the seventh example" in {
      Day9.solutionPart1("{{<!!>},{<!!>},{<!!>},{<!!>}}".toCharArray.toList) shouldBe 9
    }
    "pass the eight example" in {
      Day9.solutionPart1("{{<a!>},{<a!>},{<a!>},{<ab>}}".toCharArray.toList) shouldBe 3
    }
  }
  "The Day 9 part 2 solution" should {
    "pass the first example" in {
      Day9.solutionPart2("<>".toCharArray.toList) shouldBe 0
    }
    "pass the second example" in {
      Day9.solutionPart2("<random characters>".toCharArray.toList) shouldBe 17
    }
    "pass the third example" in {
      Day9.solutionPart2("<<<<>".toCharArray.toList) shouldBe 3
    }
    "pass the fourth example" in {
      Day9.solutionPart2("<{!>}>".toCharArray.toList) shouldBe 2
    }
    "pass the fifth example" in {
      Day9.solutionPart2("<!!>".toCharArray.toList) shouldBe 0
    }
    "pass the sixth example" in {
      Day9.solutionPart2("<!!!>>".toCharArray.toList) shouldBe 0
    }
    "pass the seventh example" in {
      Day9.solutionPart2("<{o\"i!a,<{i<a>".toCharArray.toList) shouldBe 10
    }
  }

}
