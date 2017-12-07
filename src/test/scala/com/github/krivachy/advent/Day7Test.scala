package com.github.krivachy.advent

import org.scalatest.{Matchers, WordSpec}

class Day7Test extends WordSpec with Matchers {

  "The Day 7 part 1 solution" should {
    "pass the example" in {
      Day7.solutionPart1(
        """pbga (66)
          |xhth (57)
          |ebii (61)
          |havc (66)
          |ktlj (57)
          |fwft (72) -> ktlj, cntj, xhth
          |qoyq (66)
          |padx (45) -> pbga, havc, qoyq
          |tknk (41) -> ugml, padx, fwft
          |jptl (61)
          |ugml (68) -> gyxo, ebii, jptl
          |gyxo (61)
          |cntj (57)""".stripMargin.lines.map(Day7.parseInputLine).toList
      ) shouldBe "tknk"
    }
    "pass the example with cats" in {
      Day7.solutionPart1WithCats(
        """pbga (66)
          |xhth (57)
          |ebii (61)
          |havc (66)
          |ktlj (57)
          |fwft (72) -> ktlj, cntj, xhth
          |qoyq (66)
          |padx (45) -> pbga, havc, qoyq
          |tknk (41) -> ugml, padx, fwft
          |jptl (61)
          |ugml (68) -> gyxo, ebii, jptl
          |gyxo (61)
          |cntj (57)""".stripMargin.lines.map(Day7.parseInputLine).toList
      ) shouldBe "tknk"
    }
  }

  "The Day 7 part 2 solution" should {
    "pass the example" in {
      Day7.solutionPart2(
        """pbga (66)
          |xhth (57)
          |ebii (61)
          |havc (66)
          |ktlj (57)
          |fwft (72) -> ktlj, cntj, xhth
          |qoyq (66)
          |padx (45) -> pbga, havc, qoyq
          |tknk (41) -> ugml, padx, fwft
          |jptl (61)
          |ugml (68) -> gyxo, ebii, jptl
          |gyxo (61)
          |cntj (57)""".stripMargin.lines.map(Day7.parseInputLine).toList
      ) shouldBe 60
    }
  }
}
