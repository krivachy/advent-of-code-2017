package com.github.krivachy.advent

import org.scalatest.{Matchers, WordSpec}

class Day15Test extends WordSpec with Matchers {

  "Day 15 part 1 solution" should {
    "pass the simple example" in {
      Day15.solutionPart1(65, 8921, 5) shouldBe 1
    }
    "pass the full example" in {
      Day15.solutionPart1(65, 8921, 40000000) shouldBe 588
    }
    "pass my input" in {
      Day15.solutionPart1(722, 354, 40000000) shouldBe 612
    }
  }

  "Day 15 part 2 solution" should {
    "pass the simple example" in {
      Day15.solutionPart2(65, 8921, 1056) shouldBe 1
    }
    "pass the full example" in {
      Day15.solutionPart2(65, 8921, 5000000) shouldBe 309
    }
    "pass my input" in {
      Day15.solutionPart2(722, 354, 5000000) shouldBe 285
    }
  }

}
