package com.github.krivachy.advent

import org.scalatest.{Matchers, WordSpec}

class Day13Test extends WordSpec with Matchers {

  "The Day 13 part 1 solution" should {
    "pass the example" in {
      Day13.solutionPart1(
        """0: 3
          |1: 2
          |4: 4
          |6: 4""".stripMargin.lines.map(Day13.parseLine).toSeq
      ) shouldBe 24
    }
  }

  "The Day 13 part 2 solution" should {
    "pass the example" in {
      Day13.solutionPart2(
        """0: 3
          |1: 2
          |4: 4
          |6: 4""".stripMargin.lines.map(Day13.parseLine).toSeq
      ) shouldBe 10
    }
  }

}
