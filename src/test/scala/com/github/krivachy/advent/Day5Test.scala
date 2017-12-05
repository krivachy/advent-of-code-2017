package com.github.krivachy.advent

import org.scalatest.{Matchers, WordSpec}

class Day5Test extends WordSpec with Matchers {

  "The Day 4 part 1 solution" should {
    "pass the example" in {
      Day5.solutionPart1(Seq(0, 3, 0, 1, -3)) shouldBe 5
    }
  }
  "The Day 4 part 2 solution" should {
    "pass the example" in {
      Day5.solutionPart2(Seq(0, 3, 0, 1, -3)) shouldBe 10
    }
  }
}
