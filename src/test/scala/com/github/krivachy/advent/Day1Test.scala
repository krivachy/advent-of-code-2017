package com.github.krivachy.advent

import org.scalatest.{FlatSpec, Matchers, WordSpec}

class Day1Test extends WordSpec with Matchers {

  "The day 1 part 1solution" should {
    "pass the first example" in {
      Day1.solutionPart1("1122".toList) shouldBe 3
    }
    "pass the second example" in {
      Day1.solutionPart1("1111".toList) shouldBe 4
    }
    "pass the third example" in {
      Day1.solutionPart1("1234".toList) shouldBe 0
    }
    "pass the fourth example" in {
      Day1.solutionPart1("91212129".toList) shouldBe 9
    }
  }

  "The day 1 part 2 solution" should {
    "pass the first example" in {
      Day1.solutionPart2("1212".toList) shouldBe 6
    }
    "pass the second example" in {
      Day1.solutionPart2("1221".toList) shouldBe 0
    }
    "pass the third example" in {
      Day1.solutionPart2("123425".toList) shouldBe 4
    }
    "pass the fourth example" in {
      Day1.solutionPart2("123123".toList) shouldBe 12
    }
    "pass the fifth example" in {
      Day1.solutionPart2("12131415".toList) shouldBe 4
    }
  }
}
