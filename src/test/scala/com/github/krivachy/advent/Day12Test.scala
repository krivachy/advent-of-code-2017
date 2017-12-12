package com.github.krivachy.advent

import org.scalatest.{Matchers, WordSpec}

class Day12Test extends WordSpec with Matchers {

  "The Day 12 part 1 solution" should {
    "pass the example" in {
      Day12.solutionPart1(
        """0 <-> 2
          |1 <-> 1
          |2 <-> 0, 3, 4
          |3 <-> 2, 4
          |4 <-> 2, 3, 6
          |5 <-> 6
          |6 <-> 4, 5""".stripMargin.lines.map(Day12.parseLine).toMap
      ) shouldBe 6
    }
  }

  "The Day 12 part 2 solution" should {
    "pass the example" in {
      Day12.solutionPart2(
        """0 <-> 2
          |1 <-> 1
          |2 <-> 0, 3, 4
          |3 <-> 2, 4
          |4 <-> 2, 3, 6
          |5 <-> 6
          |6 <-> 4, 5""".stripMargin.lines.map(Day12.parseLine).toMap
      ) shouldBe 2
    }
  }

}
