package com.github.krivachy.advent

import org.scalatest.{Matchers, WordSpec}

class Day6Test extends WordSpec with Matchers {
  "The Day 6 part 1 solution" should {
    "pass the example" in {
      Day6.solutionPart1(Seq(0, 2, 7, 0)) shouldBe 5
    }
  }
  "The Day 6 part 2 solution" should {
    "pass the example" in {
      Day6.solutionPart2(Seq(0, 2, 7, 0)) shouldBe 4
    }
  }
}
