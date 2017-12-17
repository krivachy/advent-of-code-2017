package com.github.krivachy.advent

import org.scalatest.{Matchers, WordSpec}

class Day16Test extends WordSpec with Matchers {

  "Day 16 part 1 solution" should {
    "pass the example" in {
      Day16.solutionPart1(
        Vector("a", "b", "c", "d", "e"),
        Day16.parseDanceMoves("s1,x3/4,pe/b")
      ) shouldBe "baedc"
    }
  }

  "Day 16 part 2 solution" should {
    "pass the example" in {
      Day16.solutionPart2(
        Vector("a", "b", "c", "d", "e"),
        2,
        Day16.parseDanceMoves("s1,x3/4,pe/b")
      ) shouldBe "ceadb"
    }
  }

}
