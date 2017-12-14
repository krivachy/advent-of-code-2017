package com.github.krivachy.advent

import org.scalatest.{Matchers, WordSpec}

class Day14Test extends WordSpec with Matchers {

  "Day 14 part 1 solution" should {
    "pass the example" in {
      Day14.solutionPart1("flqrgnkx") shouldBe 8108
    }
    "pass my input" in {
      Day14.solutionPart1("hxtvlmkl") shouldBe 8214
    }
  }

  "Day 14 part 2 solution" should {
    "pass the example" in {
      Day14.solutionPart2("flqrgnkx") shouldBe 1242
    }
    "pass the my input" in {
      Day14.solutionPart2("hxtvlmkl") shouldBe 1093
    }
  }

}
