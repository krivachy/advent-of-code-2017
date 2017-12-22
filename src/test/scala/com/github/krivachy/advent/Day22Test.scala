package com.github.krivachy.advent

import org.scalatest.{Matchers, WordSpec}

class Day22Test extends WordSpec with Matchers {

  private val exampleMap = """..#
                             |#..
                             |...""".stripMargin.lines.map(_.toVector).toVector

  "Day 22 part 1 solution" should {
    "pass the simple example" in {
      Day22.solutionPart1(70, exampleMap) shouldBe 41
    }
    "pass the complex example" in {
      Day22.solutionPart1(10000, exampleMap) shouldBe 5587
    }
  }

  "Day 22 part 2 solution" should {
    "pass the simple example" in {
      Day22.solutionPart2(100, exampleMap) shouldBe 26
    }
    "pass the complex example" in {
      Day22.solutionPart2(10000000, exampleMap) shouldBe 2511944
    }
  }
}
