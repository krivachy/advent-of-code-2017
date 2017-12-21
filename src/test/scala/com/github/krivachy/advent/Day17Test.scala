package com.github.krivachy.advent

import org.scalatest.{Matchers, WordSpec}

class Day17Test extends WordSpec with Matchers {

  "Day 17 part 1 solution" should {
    "pass the example" in {
      Day17.solutionPart1(3, 2017) shouldBe 638
    }
    "pass my input" in {
      Day17.solutionPart1(335, 2017) shouldBe 1282
    }
  }
  "Day 17 part 2 solution" should {
    "pass the my input and solution" in {
      Day17.solutionPart2(335, 50000000) shouldBe 27650600
    }
  }

}
