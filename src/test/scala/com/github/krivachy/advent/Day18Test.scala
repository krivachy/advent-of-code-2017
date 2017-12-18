package com.github.krivachy.advent

import org.scalatest.{Matchers, WordSpec}

class Day18Test extends WordSpec with Matchers {

  "Day 18 part 1 solution" should {
    "pass the example" in {
      Day18.solutionPart1(
        """set a 1
          |add a 2
          |mul a a
          |mod a 5
          |snd a
          |set a 0
          |rcv a
          |jgz a -1
          |set a 1
          |jgz a -2""".stripMargin.lines.map(Day18.parseInstructionLine).toVector
      ) shouldBe 4
    }
  }
  "Day 18 part 2 solution" should {
    "pass the example" in {
      Day18.solutionPart2(
        """snd 1
          |snd 2
          |snd p
          |rcv a
          |rcv b
          |rcv c
          |rcv d""".stripMargin.lines.map(Day18.parseInstructionLine).toVector
      ) shouldBe 3
    }
  }

}
