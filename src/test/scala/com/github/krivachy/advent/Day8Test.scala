package com.github.krivachy.advent

import org.scalatest.{Matchers, WordSpec}

class Day8Test extends WordSpec with Matchers {

  "The Day 8 part 1 solution" should {
    "pass the example" in {
      Day8.solutionPart1(
        """b inc 5 if a > 1
          |a inc 1 if b < 5
          |c dec -10 if a >= 1
          |c inc -20 if c == 10"""
          .stripMargin.lines.map(Day8.parseInputLine).toList
      ) shouldBe 1
    }
  }
  "The Day 8 part 2 solution" should {
    "pass the example" in {
      Day8.solutionPart2(
        """b inc 5 if a > 1
          |a inc 1 if b < 5
          |c dec -10 if a >= 1
          |c inc -20 if c == 10"""
          .stripMargin.lines.map(Day8.parseInputLine).toList
      ) shouldBe 10
    }
  }

}
