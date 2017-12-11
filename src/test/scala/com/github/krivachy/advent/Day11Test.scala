package com.github.krivachy.advent

import org.scalatest.{Matchers, WordSpec}

class Day11Test extends WordSpec with Matchers {
  "The Day 11 part 1 solution" should {
    "pass the first example" in {
      Day11.solutionPart1("ne,ne,ne".split(",")) shouldBe 3
    }
    "pass the second example" in {
      Day11.solutionPart1("ne,ne,sw,sw".split(",")) shouldBe 0
    }
    "pass the third example" in {
      Day11.solutionPart1("ne,ne,s,s".split(",")) shouldBe 2
    }
    "pass the fourth example" in {
      Day11.solutionPart1("se,sw,se,sw,sw".split(",")) shouldBe 3
    }
  }
}
