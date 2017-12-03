package com.github.krivachy.advent

import org.scalatest.{Matchers, WordSpec}

class Day3Test extends WordSpec with Matchers {

  "The day 3 part 1 solution" should {
    "pass the first example" in {
      Day3.solutionPart1(1) shouldBe 0
    }
    "pass the second example" in {
      Day3.solutionPart1(12) shouldBe 3
    }
    "pass the third example" in {
      Day3.solutionPart1(23) shouldBe 2
    }
    "pass the fourth example" in {
      Day3.solutionPart1(1024) shouldBe 31
    }
    "pass for my input" in {
      Day3.solutionPart1(347991) shouldBe 480
    }
  }
}
